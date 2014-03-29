use 5.008003;
use strict;
use warnings;
no warnings qw( void once uninitialized );

package Sub::Accessor::Small;

use Carp             qw( carp croak );
use Eval::TypeTiny   qw();
use Exporter::Tiny   qw();
use Hash::FieldHash  qw( fieldhash );
use Scalar::Util     qw( blessed reftype );

BEGIN {
	*HAS_SUB_NAME = eval { require Sub::Name } ? sub(){1} : sub(){0};
};

our $AUTHORITY = 'cpan:TOBYINK';
our $VERSION   = '0.006';
our @ISA       = qw/ Exporter::Tiny /;

fieldhash( our %FIELDS );

sub _generate_has : method
{
	my $me = shift;
	my (undef, undef, $export_opts) = @_;
	
	my $code = sub { $me->has($export_opts, @_) };
	$code = Sub::Name::subname("$me\::has", $code)
		if HAS_SUB_NAME;
	return $code;
}

my $uniq = 0;
sub has : method
{
	my $me = shift;
	my $export_opts = ref($_[0]) eq 'HASH' ? shift(@_) : {};
	my ($name, %opts) = (@_%2) ? @_ : (undef, @_);
	
	# Massage %opts
	$opts{_export} = $export_opts;
	$opts{package} = $export_opts->{into} if defined($export_opts->{into}) && !ref($export_opts->{into});
	$me->canonicalize_opts($name, ++$uniq, \%opts);
	
	for my $type (qw( accessor reader writer predicate clearer ))
	{
		if (defined $opts{$type})
		{
			$me->install_coderef(
				$opts{$type},
				$me->$type($name, $uniq, \%opts),
				$name, $uniq, \%opts,
			);
		}
	}
	
	if (defined $opts{handles})
	{
		my @pairs = $me->expand_handles($opts{handles}, $name, $uniq, \%opts);
		while (@pairs)
		{
			my ($target, $method) = splice(@pairs, 0, 2);
			$me->install_coderef(
				$target,
				$me->handles($method, $name, $uniq, \%opts),
				$name, $uniq, \%opts,
			);
		}
	}
	
	my @return = 
		$opts{is} eq 'ro'   ? ($opts{reader}) :
		$opts{is} eq 'rw'   ? ($opts{accessor}) :
		$opts{is} eq 'rwp'  ? ($opts{reader}, $opts{writer}) :
		$opts{is} eq 'lazy' ? ($opts{reader}) :
		();
	wantarray ? @return : $return[0];
}

sub install_coderef
{
	shift;
	my ($target, $coderef, $name, $uniq, $opts) = @_;
	
	return unless defined $target;
	
	if (!ref $target and $target =~ /\A[^\W0-9]\w+\z/)
	{
		my $name = "$opts->{package}\::$target";
		$coderef = Sub::Name::subname($name, $coderef) if HAS_SUB_NAME;
		no strict qw(refs);
		*$name = $coderef;
		return;
	}
		
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

sub expand_handles
{
	shift;
	my ($handles, $name, $uniq, $opts) = @_;
	
	if (ref($handles) eq q(ARRAY))
	{
		return @$handles;
	}
	
	croak "Expected delegations to be a reference to an array; got $handles";
}

{
	my %one = (
		accessor   => [qw/ %s %s /],
		reader     => [qw/ get_%s _get%s /],
		writer     => [qw/ set_%s _set%s /],
		predicate  => [qw/ has_%s _has%s /],
		clearer    => [qw/ clear_%s _clear%s /],
		trigger    => [qw/ _trigger_%s _trigger_%s /],
		builder    => [qw/ _builder_%s _builder_%s /],
	);
	
	sub canonicalize_1 : method
	{
		my $me = shift;
		my ($name, $uniq, $opts) = @_;
		
		my $is_private = ($name =~ /\A_/) ? 1 : 0;
		
		for my $type (keys %one)
		{
			next if !exists($opts->{$type});
			next if ref($opts->{$type});
			next if $opts->{$type} ne 1;
			
			croak("Cannot interpret $type=>1 because attribute has no name defined")
				if !defined $name;
			
			$opts->{$type} = sprintf($one{$type}[$is_private], $name);
		}
	}
	
	sub canonicalize_builder : method
	{
		my $me = shift;
		my ($name, $uniq, $opts) = @_;
			
		if (ref $opts->{builder} eq 'CODE')
		{
			HAS_SUB_NAME or do { require Sub::Name };
			
			my $code = $opts->{builder};
			defined($name) && defined($opts->{package})
				or croak("Invalid builder; expected method name as string");
			
			my $is_private = ($name =~ /\A_/) ? 1 : 0;
			
			my $subname    = sprintf($one{builder}[$is_private], $name);
			my $fq_subname = "$opts->{package}\::$name";
			$me->_exporter_install_sub(
				$subname,
				{},
				$opts->{_export},
				Sub::Name::subname($fq_subname, $code),
			);
		}
	}
}

sub canonicalize_is : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	if ($opts->{is} eq 'rw')
	{
		$opts->{accessor} = $name
			if !exists($opts->{accessor}) and defined $name;
	}
	elsif ($opts->{is} eq 'ro')
	{
		$opts->{reader} = $name
			if !exists($opts->{reader}) and defined $name;
	}
	elsif ($opts->{is} eq 'rwp')
	{
		$opts->{reader} = $name
			if !exists($opts->{reader}) and defined $name;
		$opts->{writer} = "_set_$name"
			if !exists($opts->{writer}) and defined $name;
	}
	elsif ($opts->{is} eq 'lazy')
	{
		$opts->{reader} = $name
			if !exists($opts->{reader}) and defined $name;
		$opts->{lazy} = 1
			if !exists($opts->{lazy});
		$opts->{builder} = 1
			unless $opts->{builder} || $opts->{default};
	}
}

sub canonicalize_default : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	return unless exists($opts->{default});
	
	if (not ref $opts->{default})
	{
		my $value = $opts->{default};
		$opts->{default} = sub { $value };
	}
	
	croak("Invalid default; expected a CODE ref")
		unless ref $opts->{default} ne 'CODE';
}

sub canonicalize_isa : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
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
}

sub canonicalize_trigger : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	if (defined $opts->{trigger} and not ref $opts->{trigger})
	{
		my $method_name = $opts->{trigger};
		$opts->{trigger} = sub { my $self = shift; $self->$method_name(@_) };
	}
}

sub canonicalize_opts : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	croak("Initializers are not supported") if $opts->{initializer};
	croak("Traits are not supported") if $opts->{traits};
	croak("The lazy_build option is not supported") if $opts->{lazy_build};
	
	$me->canonicalize_1(@_);
	$me->canonicalize_is(@_);
	$me->canonicalize_isa(@_);
	$me->canonicalize_default(@_);
	$me->canonicalize_builder(@_);
	$me->canonicalize_trigger(@_);
}

sub accessor_kind : method
{
	return 'small';
}

sub inline_to_coderef : method
{
	my $me = shift;
	my ($method_type, $code, $name, $uniq, $opts) = @_;
	
	my $kind = $me->accessor_kind;
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

sub clearer : method
{
	my $me = shift;
	
	$me->inline_to_coderef(
		clearer => $me->inline_clearer(@_),
		@_,
	);
}

sub inline_clearer : method
{
	my $me = shift;
	sprintf(
		q[ delete(%s) ],
		$me->inline_access(@_),
	);
}

sub inline_access : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	sprintf(
		q[ $Sub::Accessor::Small::FIELDS{$_[0]}{%d} ],
		$uniq,
	);
}

sub inline_access_w : method
{
	my $me = shift;
	my ($name, $uniq, $opts, $expr) = @_;
	sprintf(
		q[ %s = %s ],
		$me->inline_access($name, $uniq, $opts),
		$expr,
	);
}

sub predicate : method
{
	my $me = shift;
	
	$me->inline_to_coderef(
		predicate => $me->inline_predicate(@_),
		@_,
	);
}

sub inline_predicate : method
{
	my $me = shift;
	sprintf(
		q[ exists(%s) ],
		$me->inline_access(@_),
	);
}

sub handles : method
{
	my $me = shift;
	my ($method, $name, $uniq, $opts) = @_;
	
	$me->inline_to_coderef(
		'delegated method' => $me->inline_handles(@_),
		@_[1 .. 3],
	);
}

my $handler_uniq = 0;
sub inline_handles : method
{
	my $me = shift;
	my ($method, $name, $uniq, $opts) = @_;
	
	my $get = $me->inline_access($name, $uniq, $opts);
	
	my $varname = sprintf('$handler_%d', ++$handler_uniq);
	$opts->{inline_environment}{$varname} = \($method);
	
	my $death = 'Scalar::Util::blessed($h) or Carp::croak("Expected blessed object to delegate to; got $h")';
	
	if (ref $method eq 'ARRAY')
	{
		return sprintf(
			q[ %s; my $h = %s; %s; shift; my ($m, @a) = @%s; $h->$m(@a, @_) ],
			$me->inline_default($name, $uniq, $opts),
			$get,
			$death,
			$varname,
		);
	}
	else
	{
		return sprintf(
			q[ %s; my $h = %s; %s; shift; $h->%s(@_) ],
			$me->inline_default($name, $uniq, $opts),
			$get,
			$death,
			$varname,
		);
	}
}

sub inline_get : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	my $get = $me->inline_access(@_);
	
	if ($opts->{auto_deref})
	{
		$get = sprintf(
			q[ do { my $x = %s; wantarray ? (ref($x) eq 'ARRAY' ? @$x : ref($x) eq 'HASH' ? %$x : $x ) : $x } ],
			$get,
		);
	}
	
	return $get;
}

sub inline_default : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	if ($opts->{lazy})
	{
		my $get = $me->inline_access(@_);
		
		if ($opts->{default})
		{
			$opts->{inline_environment}{'$default'} = \($opts->{default});
			
			return sprintf(
				q[ %s unless %s; ],
				$me->inline_access_w($name, $uniq, $opts, q[$default->($_[0])]),
				$me->inline_predicate($name, $uniq, $opts),
			);
		}
		elsif (defined $opts->{builder})
		{
			return sprintf(
				q[ %s unless %s; ],
				$me->inline_access_w($name, $uniq, $opts, q($_[0]->).$opts->{builder}),
				$me->inline_predicate($name, $uniq, $opts),
			);
		}
	}
	
	return '';
}

sub reader : method
{
	my $me = shift;
	
	$me->inline_to_coderef(
		reader => $me->inline_reader(@_),
		@_,
	);
}

sub inline_reader : method
{
	my $me = shift;
	
	join('',
		$me->inline_default(@_),
		$me->inline_get(@_),
	);
}

sub writer : method
{
	my $me = shift;
	
	$me->inline_to_coderef(
		writer => $me->inline_writer(@_),
		@_,
	);
}

sub inline_writer : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	my $get    = $me->inline_access(@_);
	my $coerce = $me->inline_type_coercion('$_[1]', @_);
	
	if ($coerce eq '$_[1]')  # i.e. no coercion
	{
		if (!$opts->{trigger} and !$opts->{weak_ref})
		{
			return $me->inline_access_w(
				$name, $uniq, $opts,
				$me->inline_type_assertion('$_[1]', @_),
			);
		}
		
		return sprintf(
			'%s; %s; %s; %s; %s',
			$me->inline_type_assertion('$_[1]', @_),
			$me->inline_trigger('$_[1]', $get, @_),
			$me->inline_access_w(
				$name, $uniq, $opts,
				'$_[1]',
			),
			$me->inline_weaken(@_),
			$me->inline_get(@_),
		);
	}
	
	sprintf(
		'my $val = %s; %s; %s; %s; %s; $val',
		$coerce,
		$me->inline_type_assertion('$val', @_),
		$me->inline_trigger('$val', $get, @_),
		$me->inline_access_w(
			$name, $uniq, $opts,
			'$val',
		),
		$me->inline_weaken(@_),
	);
}

sub accessor : method
{
	my $me = shift;
	
	$me->inline_to_coderef(
		accessor => $me->inline_accessor(@_),
		@_,
	);
}

sub inline_accessor : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	my $get    = $me->inline_access(@_);
	my $coerce = $me->inline_type_coercion('$_[1]', @_);
	
	if ($coerce eq '$_[1]')  # i.e. no coercion
	{
		if (!$opts->{trigger} and !$opts->{weak_ref})
		{
			return sprintf(
				'(@_ > 1) ? (%s) : %s',
				$me->inline_access_w(
					$name, $uniq, $opts,
					$me->inline_type_assertion('$_[1]', @_),
				),
				$get,
			);
		}
		
		return sprintf(
			'if (@_ > 1) { %s; %s; %s; %s }; %s',
			$me->inline_type_assertion('$_[1]', @_),
			$me->inline_trigger('$_[1]', $get, @_),
			$me->inline_access_w(
				$name, $uniq, $opts,
				'$_[1]',
			),
			$me->inline_weaken(@_),
			$me->inline_get(@_),
		);
	}
	
	sprintf(
		'if (@_ > 1) { my $val = %s; %s; %s; %s; %s }; %s',
		$coerce,
		$me->inline_type_assertion('$val', @_),
		$me->inline_trigger('$val', $get, @_),
		$me->inline_access_w(
			$name, $uniq, $opts,
			'$val',
		),
		$me->inline_weaken(@_),
		$me->inline_get(@_),
	);
}

sub inline_type_coercion : method
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

sub inline_type_assertion : method
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

sub inline_weaken : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	return '' unless $opts->{weak_ref};
	
	sprintf(
		q[ Scalar::Util::weaken(%s) if ref(%s) ],
		$me->inline_access(@_),
		$me->inline_access(@_),
	);
}

sub inline_trigger : method
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

