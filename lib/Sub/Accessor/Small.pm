use 5.008003;
use strict;
use warnings;
no warnings qw( void once uninitialized );

package Sub::Accessor::Small;

use Carp qw( carp croak );
use Eval::TypeTiny ();
use Exporter::Tiny ();
use Hash::FieldHash qw( fieldhash );
use Scalar::Util qw( blessed reftype );

BEGIN {
	*HAS_SUB_NAME = eval { require Sub::Name } ? sub(){1} : sub(){0};
};

our $AUTHORITY = 'cpan:TOBYINK';
our $VERSION   = '0.005';
our @ISA       = qw/ Exporter::Tiny /;

fieldhash( our %FIELDS );

my $uniq = 0;
sub has : method
{
	my $me = shift;
	my $export_opts = ref($_[0]) eq 'HASH' ? shift(@_) : {};
	my ($name, %opts) = (@_%2) ? @_ : (undef, @_);
	
	# Massage %opts
	$opts{_export} = $export_opts;
	$opts{package} = $export_opts->{into} if defined($export_opts->{into}) && !ref($export_opts->{into});
	$me->_canonicalize_opts($name, ++$uniq, \%opts);
	
	my @return;
	my $is = $opts{is};
	
	if (exists $opts{clearer})
	{
		$me->_install_coderef(
			$opts{clearer},
			$me->_clearer($name, $uniq, \%opts),
			$name, $uniq, \%opts,
		);
	}
	
	if (exists $opts{predicate})
	{
		$me->_install_coderef(
			$opts{predicate},
			$me->_predicate($name, $uniq, \%opts),
			$name, $uniq, \%opts,
		);
	}
	
	if (exists $opts{handles})
	{
		my @pairs = $me->_expand_handles($opts{handles}, $name, $uniq, \%opts);
		while (@pairs)
		{
			my ($target, $method) = splice(@pairs, 0, 2);
			$me->_install_coderef(
				$target,
				$me->_handles($method, $name, $uniq, \%opts),
				$name, $uniq, \%opts,
			);
		}
	}
	
	if (exists $opts{reader} or $is eq 'ro' or $is eq 'rwp')
	{
		my $reader = $me->_reader($name, $uniq, \%opts);
		$me->_install_coderef($opts{reader}, $reader, $name, $uniq, \%opts)
			if exists $opts{reader};
		push @return, $reader if ($is eq 'ro' or $is eq 'rwp');
	}
	
	if (exists $opts{writer} or $is eq 'rwp')
	{
		my $writer = $me->_writer($name, $uniq, \%opts);
		$me->_install_coderef($opts{writer}, $writer, $name, $uniq, \%opts)
			if exists $opts{writer};
		push @return, $writer if $is eq 'rwp';
	}
	
	if (exists $opts{accessor} or $is eq 'rw')
	{
		my $accessor = $me->_accessor($name, $uniq, \%opts);
		$me->_install_coderef($opts{accessor}, $accessor, $name, $uniq, \%opts)
			if exists $opts{accessor};
		push @return, $accessor;
	}
	
	wantarray ? @return : $return[0];
}

sub _install_coderef
{
	shift;
	my ($target, $coderef) = @_;
	
	if (ref($target) eq q(SCALAR) and not defined $$target)
	{
		$$target = $coderef;
		return;
	}
	
	if (!ref($target) and $target eq 1)
	{
		return;
	}
	
	croak "Expected installation target to be a reference to an undefined scalar; got $target";
}

sub _expand_handles
{
	shift;
	my ($handles, $name, $uniq, $opts) = @_;
	
	if (ref($handles) eq q(ARRAY))
	{
		return @$handles;
	}
	
	croak "Expected delegations to be a reference to an array; got $handles";
}

sub _canonicalize_opts : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	$opts->{is} ||= 'rw';
	
	if ($opts->{is} eq 'lazy')
	{
		$opts->{is}      = 'ro';
		$opts->{builder} = 1 unless $opts->{builder} || $opts->{default};
	}
	
	croak("Initializers are not supported") if $opts->{initializer};
	croak("Traits are not supported") if $opts->{traits};
	croak("The lazy_build option is not supported") if $opts->{lazy_build};
	
	if (defined $opts->{default} and not ref $opts->{default})
	{
		my $value = $opts->{default};
		$opts->{default} = sub { $value };
	}
	
	if (defined $opts->{default} and ref $opts->{default} ne 'CODE')
	{
		croak("Invalid default; expected a CODE ref");
	}
	
	if (my $does = $opts->{does})
	{
		$opts->{isa} ||= sub {
			blessed($_[0]) && $_[0]->DOES($does)
		};
	}
	
	if (defined $opts->{isa} and not ref $opts->{isa})
	{
		my $type_name = $opts->{isa};
		eval { require Type::Utils }
			or croak("Missing requirement; type constraint strings require Type::Utils");
		
		$opts->{isa} = $opts->{package}
			? Type::Utils::dwim_type($type_name, for => $opts->{package})
			: Type::Utils::dwim_type($type_name);
	}
	
	if (ref $opts->{builder} eq 'CODE')
	{
		HAS_SUB_NAME or do { require Sub::Name };
		
		my $code = $opts->{builder};
		defined($name) && defined($opts->{package})
			or croak("Invalid builder; expected method name as string");
		
		my $qname = "$opts->{package}\::_build_$name";
		$me->_exporter_install_sub(
			"_build_$name",
			{},
			$opts->{_export},
			Sub::Name::subname($qname, $code),
		);
	}
	elsif ($opts->{builder} eq '1')
	{
		defined($name)
			or croak("Invalid builder; cannot determine method name");
		$opts->{builder} = "_build_$name";
	}
	
	if (defined $opts->{trigger} and not ref $opts->{trigger})
	{
		my $method_name = $opts->{trigger};
		$opts->{trigger} = sub { my $self = shift; $self->$method_name(@_) };
	}
}

sub _accessor_kind : method
{
	return 'small';
}

sub _inline_to_coderef : method
{
	my $me = shift;
	my ($method_type, $code, $name, $uniq, $opts) = @_;
	
	my $kind = $me->_accessor_kind;
	my $src  = sprintf(q[sub { %s }], $code);
	my $desc = defined($name)
		? sprintf('%s %s for %s', $kind, $method_type, $name)
		: sprintf('%s %s', $kind, $method_type);
	# warn "#### $desc\n$src\n";
	
	return Eval::TypeTiny::eval_closure(
		source      => $src,
		environment => $opts->{inline_environment},
		description => $desc,
	);
}

sub _clearer : method
{
	my $me = shift;
	
	$me->_inline_to_coderef(
		clearer => $me->_inline_clearer(@_),
		@_,
	);
}

sub _inline_clearer : method
{
	my $me = shift;
	sprintf(
		q[ delete(%s) ],
		$me->_inline_access(@_),
	);
}

sub _inline_access : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	sprintf(
		q[ $Sub::Accessor::Small::FIELDS{$_[0]}{%d} ],
		$uniq,
	);
}

sub _inline_access_w : method
{
	my $me = shift;
	my ($name, $uniq, $opts, $expr) = @_;
	sprintf(
		q[ %s = %s ],
		$me->_inline_access($name, $uniq, $opts),
		$expr,
	);
}

sub _predicate : method
{
	my $me = shift;
	
	$me->_inline_to_coderef(
		predicate => $me->_inline_predicate(@_),
		@_,
	);
}

sub _inline_predicate : method
{
	my $me = shift;
	sprintf(
		q[ exists(%s) ],
		$me->_inline_access(@_),
	);
}

sub _handles : method
{
	my $me = shift;
	my ($method, $name, $uniq, $opts) = @_;
	
	$me->_inline_to_coderef(
		'delegated method' => $me->_inline_handles(@_),
		@_[1 .. 3],
	);
}

my $handler_uniq = 0;
sub _inline_handles : method
{
	my $me = shift;
	my ($method, $name, $uniq, $opts) = @_;
	
	my $get = $me->_inline_access($name, $uniq, $opts);
	
	my $varname = sprintf('$handler_%d', ++$handler_uniq);
	$opts->{inline_environment}{$varname} = \($method);
	
	my $death = 'Scalar::Util::blessed($h) or Carp::croak("Expected blessed object to delegate to; got $h")';
	
	if (ref $method eq 'ARRAY')
	{
		return sprintf(
			q[ %s; my $h = %s; %s; shift; my ($m, @a) = @%s; $h->$m(@a, @_) ],
			$me->_inline_default($name, $uniq, $opts),
			$get,
			$death,
			$varname,
		);
	}
	else
	{
		return sprintf(
			q[ %s; my $h = %s; %s; shift; $h->%s(@_) ],
			$me->_inline_default($name, $uniq, $opts),
			$get,
			$death,
			$varname,
		);
	}
}

sub _inline_get : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	my $get = $me->_inline_access(@_);
	
	if ($opts->{auto_deref})
	{
		$get = sprintf(
			q[ do { my $x = %s; wantarray ? (ref($x) eq 'ARRAY' ? @$x : ref($x) eq 'HASH' ? %$x : $x ) : $x } ],
			$get,
		);
	}
	
	return $get;
}

sub _inline_default : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	if ($opts->{lazy})
	{
		my $get = $me->_inline_access(@_);
		
		if ($opts->{default})
		{
			$opts->{inline_environment}{'$default'} = \($opts->{default});
			
			return sprintf(
				q[ %s unless %s; ],
				$me->_inline_access_w($name, $uniq, $opts, q[$default->($_[0])]),
				$me->_inline_predicate($name, $uniq, $opts),
			);
		}
		elsif (defined $opts->{builder})
		{
			return sprintf(
				q[ %s unless %s; ],
				$me->_inline_access_w($name, $uniq, $opts, q($_[0]->).$opts->{builder}),
				$me->_inline_predicate($name, $uniq, $opts),
			);
		}
	}
	
	return '';
}

sub _reader : method
{
	my $me = shift;
	
	$me->_inline_to_coderef(
		reader => $me->_inline_reader(@_),
		@_,
	);
}

sub _inline_reader : method
{
	my $me = shift;
	
	join('',
		$me->_inline_default(@_),
		$me->_inline_get(@_),
	);
}

sub _writer : method
{
	my $me = shift;
	
	$me->_inline_to_coderef(
		writer => $me->_inline_writer(@_),
		@_,
	);
}

sub _inline_writer : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	my $get    = $me->_inline_access(@_);
	my $coerce = $me->_inline_type_coercion('$_[1]', @_);
	
	if ($coerce eq '$_[1]')  # i.e. no coercion
	{
		if (!$opts->{trigger} and !$opts->{weak_ref})
		{
			return $me->_inline_access_w(
				$name, $uniq, $opts,
				$me->_inline_type_assertion('$_[1]', @_),
			);
		}
		
		return sprintf(
			'%s; %s; %s; %s; %s',
			$me->_inline_type_assertion('$_[1]', @_),
			$me->_inline_trigger('$_[1]', $get, @_),
			$me->_inline_access_w(
				$name, $uniq, $opts,
				'$_[1]',
			),
			$me->_inline_weaken(@_),
			$me->_inline_get(@_),
		);
	}
	
	sprintf(
		'my $val = %s; %s; %s; %s; %s; $val',
		$coerce,
		$me->_inline_type_assertion('$val', @_),
		$me->_inline_trigger('$val', $get, @_),
		$me->_inline_access_w(
			$name, $uniq, $opts,
			'$val',
		),
		$me->_inline_weaken(@_),
	);
}

sub _accessor : method
{
	my $me = shift;
	
	$me->_inline_to_coderef(
		accessor => $me->_inline_accessor(@_),
		@_,
	);
}

sub _inline_accessor : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	my $get    = $me->_inline_access(@_);
	my $coerce = $me->_inline_type_coercion('$_[1]', @_);
	
	if ($coerce eq '$_[1]')  # i.e. no coercion
	{
		if (!$opts->{trigger} and !$opts->{weak_ref})
		{
			return sprintf(
				'(@_ > 1) ? (%s) : %s',
				$me->_inline_access_w(
					$name, $uniq, $opts,
					$me->_inline_type_assertion('$_[1]', @_),
				),
				$get,
			);
		}
		
		return sprintf(
			'if (@_ > 1) { %s; %s; %s; %s }; %s',
			$me->_inline_type_assertion('$_[1]', @_),
			$me->_inline_trigger('$_[1]', $get, @_),
			$me->_inline_access_w(
				$name, $uniq, $opts,
				'$_[1]',
			),
			$me->_inline_weaken(@_),
			$me->_inline_get(@_),
		);
	}
	
	sprintf(
		'if (@_ > 1) { my $val = %s; %s; %s; %s; %s }; %s',
		$coerce,
		$me->_inline_type_assertion('$val', @_),
		$me->_inline_trigger('$val', $get, @_),
		$me->_inline_access_w(
			$name, $uniq, $opts,
			'$val',
		),
		$me->_inline_weaken(@_),
		$me->_inline_get(@_),
	);
}

sub _inline_type_coercion : method
{
	my $me = shift;
	my ($var, $name, $uniq, $opts) = @_;
	
	my $coercion = $opts->{coerce} or return $var;
	
	unless (ref $coercion)
	{
		my $type = $opts->{isa};
		
		if (blessed($type) and $type->can('coercion'))
		{
			$coercion = $type->coercion;
		}
		elsif (blessed($type) and $type->can('coerce'))
		{
			$coercion = sub { $type->coerce(@_) };
		}
		else
		{
			croak("Invalid coerce; type constraint cannot be probed for coercion");
		}
		
		unless (ref $coercion)
		{
			carp("Invalid coerce; type constraint has no coercion");
			return $var;
		}
	}
	
	if ( blessed($coercion)
	and $coercion->can('can_be_inlined')
	and $coercion->can_be_inlined
	and $coercion->can('inline_coercion') )
	{
		return $coercion->inline_coercion($var);
	}
	
	# Otherwise need to close over $coerce
	$opts->{inline_environment}{'$coercion'} = \$coercion;
	
	if ( blessed($coercion)
	and $coercion->can('coerce') )
	{
		return sprintf('$coercion->coerce(%s)', $var);
	}
	
	return sprintf('$coercion->(%s)', $var);
}

sub _inline_type_assertion : method
{
	my $me = shift;
	my ($var, $name, $uniq, $opts) = @_;
	
	my $type = $opts->{isa} or return $var;
	
	if ( blessed($type)
	and $type->isa('Type::Tiny')
	and $type->can_be_inlined )
	{
		my $ass = $type->inline_assert($var);
		if ($ass =~ /\Ado \{(.+)\};\z/sm)
		{
			return "do { $1 }";  # i.e. drop trailing ";"
		}
		# otherwise protect expression from trailing ";"
		return "do { $ass }"
	}
	
	# Otherwise need to close over $type
	$opts->{inline_environment}{'$type'} = \$type;
	
	# non-Type::Tiny but still supports inlining
	if ( blessed($type)
	and $type->can('can_be_inlined')
	and $type->can_be_inlined )
	{
		my $inliner = $type->can('inline_check') || $type->can('_inline_check');
		if ($inliner)
		{
			return sprintf('do { %s } ? %s : Carp::croak($type->get_message(%s))', $type->$inliner($var), $var, $var);
		}
	}
	
	if ( blessed($type)
	and $type->can('check')
	and $type->can('get_message') )
	{
		return sprintf('$type->check(%s) ? %s : Carp::croak($type->get_message(%s))', $var, $var, $var);
	}
	
	return sprintf('$type->(%s) ? %s : Carp::croak("Value %s failed type constraint check")', $var, $var, $var);
}

sub _inline_weaken : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	return '' unless $opts->{weak_ref};
	
	sprintf(
		q[ Scalar::Util::weaken(%s) if ref(%s) ],
		$me->_inline_access(@_),
		$me->_inline_access(@_),
	);
}

sub _inline_trigger : method
{
	my $me = shift;
	my ($new, $old, $name, $uniq, $opts) = @_;
	
	my $trigger = $opts->{trigger} or return '';
	
	$opts->{inline_environment}{'$trigger'} = \$trigger;
	return sprintf('$trigger->($_[0], %s, %s)', $new, $old);
}

1;

__END__

=pod

=encoding utf-8

=for stopwords benchmarking

=head1 NAME

Sub::Accessor::Small - base class used by Lexical::Accessor

=head1 DESCRIPTION

Not documented yet.

=head1 BUGS

Please report any bugs to
L<http://rt.cpan.org/Dist/Display.html?Queue=Lexical-Accessor>.

=head1 SUPPORT

Using this module directly is currently unsupported.

=head1 SEE ALSO

L<Lexical::Accessor>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2013-2014 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=head1 DISCLAIMER OF WARRANTIES

THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

