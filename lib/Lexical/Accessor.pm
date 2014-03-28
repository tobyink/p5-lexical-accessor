use 5.008003;
use strict;
use warnings;
no warnings qw( void once uninitialized );

use Eval::TypeTiny ();
use Exporter::Tiny ();

package Lexical::Accessor;

use Carp qw( carp croak );
use Hash::FieldHash qw( fieldhash );
use Scalar::Util qw( blessed reftype );

BEGIN {
	*HAS_SUB_NAME = eval { require Sub::Name } ? sub(){1} : sub(){0};
};

our $AUTHORITY = 'cpan:TOBYINK';
our $VERSION   = '0.003';
our @EXPORT    = qw/ lexical_has /;
our @ISA       = qw/ Exporter::Tiny /;

fieldhash( our %FIELDS );

sub _generate_lexical_has : method
{
	my $me = shift;
	my (undef, undef, $export_opts) = @_;
	
	my $code = sub { $me->lexical_has($export_opts, @_) };
	$code = Sub::Name::subname("$me\::lexical_has", $code) if HAS_SUB_NAME;
	return $code;
}

my $uniq = 0;
sub lexical_has : method
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
			$me->_lexical_clearer($name, $uniq, \%opts),
			$name, $uniq, \%opts,
		);
	}
	
	if (exists $opts{predicate})
	{
		$me->_install_coderef(
			$opts{predicate},
			$me->_lexical_predicate($name, $uniq, \%opts),
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
				$me->_lexical_handles($method, $name, $uniq, \%opts),
				$name, $uniq, \%opts,
			);
		}
	}
	
	if (exists $opts{reader} or $is eq 'ro' or $is eq 'rwp')
	{
		my $reader = $me->_lexical_reader($name, $uniq, \%opts);
		$me->_install_coderef($opts{reader}, $reader, $name, $uniq, \%opts)
			if exists $opts{reader};
		push @return, $reader if ($is eq 'ro' or $is eq 'rwp');
	}
	
	if (exists $opts{writer} or $is eq 'rwp')
	{
		my $writer = $me->_lexical_writer($name, $uniq, \%opts);
		$me->_install_coderef($opts{writer}, $writer, $name, $uniq, \%opts)
			if exists $opts{writer};
		push @return, $writer if $is eq 'rwp';
	}
	
	if (exists $opts{accessor} or $is eq 'rw')
	{
		my $accessor = $me->_lexical_accessor($name, $uniq, \%opts);
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
	
	if (defined $opts->{init_arg})
	{
		croak("Invalid init_arg; private attributes cannot be initialized in the constructor");
	}
	
	if ($opts->{required})
	{
		croak("Invalid required; private attributes cannot be initialized in the constructor");
	}
	
	if (defined $opts->{default} and not ref $opts->{default})
	{
		my $value = $opts->{default};
		$opts->{default} = sub { $value };
	}
	
	if (defined $opts->{default} and ref $opts->{default} ne 'CODE')
	{
		croak("Invalid default; expected a CODE ref");
	}
	
	if (defined $opts->{lazy} and not $opts->{lazy})
	{
		croak("Invalid lazy; private attributes cannot be eager");
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

sub _inline_to_coderef : method
{
	my $me = shift;
	my ($method_type, $code, $name, $uniq, $opts) = @_;
	
	my $src  = sprintf(q[sub { %s }], $code);
	my $desc = defined($name)
		? sprintf('lexical %s for %s', $method_type, $name)
		: sprintf('lexical %s', $method_type);
	
	my $coderef = Eval::TypeTiny::eval_closure(
		source      => $src,
		environment => $opts->{inline_environment},
		description => $desc,
	);
	
	HAS_SUB_NAME && $opts->{package} && defined($name)
		? Sub::Name::subname("$opts->{package}\::__LEXICAL__[$name]", $coderef)
		: $coderef
}

sub _lexical_clearer : method
{
	my $me = shift;
	
	$me->_inline_to_coderef(
		clearer => $me->_inline_lexical_clearer(@_),
		@_,
	);
}

sub _inline_lexical_clearer : method
{
	my $me = shift;
	sprintf(
		q[ delete(%s) ],
		$me->_inline_lexical_access(@_),
	);
}

sub _inline_lexical_access : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	sprintf(
		q[ $Lexical::Accessor::FIELDS{$_[0]}{%d} ],
		$uniq,
	);
}

sub _lexical_predicate : method
{
	my $me = shift;
	
	$me->_inline_to_coderef(
		predicate => $me->_inline_lexical_predicate(@_),
		@_,
	);
}

sub _inline_lexical_predicate : method
{
	my $me = shift;
	sprintf(
		q[ exists(%s) ],
		$me->_inline_lexical_access(@_),
	);
}

sub _lexical_handles : method
{
	my $me = shift;
	my ($method, $name, $uniq, $opts) = @_;
	
	$me->_inline_to_coderef(
		'delegated method' => $me->_inline_lexical_handles(@_),
		@_[1 .. 3],
	);
}

my $handler_uniq = 0;
sub _inline_lexical_handles : method
{
	my $me = shift;
	my ($method, $name, $uniq, $opts) = @_;
	
	my $get = $me->_inline_lexical_access($name, $uniq, $opts);
	
	my $varname = sprintf('$handler_%d', ++$handler_uniq);
	$opts->{inline_environment}{$varname} = \($method);
	
	my $death = 'Scalar::Util::blessed($h) or Carp::croak("Expected blessed object to delegate to; got $h")';
	
	if (ref $method eq 'ARRAY')
	{
		return sprintf(
			q[ %s; my $h = %s; %s; shift; my ($m, @a) = @%s; $h->$m(@a, @_) ],
			$me->_inline_lexical_default($name, $uniq, $opts),
			$get,
			$death,
			$varname,
		);
	}
	else
	{
		return sprintf(
			q[ %s; my $h = %s; %s; shift; $h->%s(@_) ],
			$me->_inline_lexical_default($name, $uniq, $opts),
			$get,
			$death,
			$varname,
		);
	}
}

sub _inline_lexical_get : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	my $get = $me->_inline_lexical_access(@_);
	
	if ($opts->{auto_deref})
	{
		$get = sprintf(
			q[ do { my $x = %s; wantarray ? (ref($x) eq 'ARRAY' ? @$x : ref($x) eq 'HASH' ? %$x : $x ) : $x } ],
			$get,
		);
	}
	
	return $get;
}

sub _inline_lexical_default : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	if ($opts->{lazy})
	{
		my $get = $me->_inline_lexical_access(@_);
		
		if ($opts->{default})
		{
			$opts->{inline_environment}{'$default'} = \($opts->{default});
			
			return sprintf(
				q[ %s = $default->($_[0]) unless exists(%s); ],
				$get,
				$get,
			);
		}
		elsif (defined $opts->{builder})
		{
			return sprintf(
				q[ %s = $_[0]->%s unless exists(%s); ],
				$get,
				$opts->{builder},
				$get,
			);
		}
	}
	
	return '';
}

sub _lexical_reader : method
{
	my $me = shift;
	
	$me->_inline_to_coderef(
		reader => $me->_inline_lexical_reader(@_),
		@_,
	);
}

sub _inline_lexical_reader : method
{
	my $me = shift;
	
	join('',
		$me->_inline_lexical_default(@_),
		$me->_inline_lexical_get(@_),
	);
}

sub _lexical_writer : method
{
	my $me = shift;
	
	$me->_inline_to_coderef(
		writer => $me->_inline_lexical_writer(@_),
		@_,
	);
}

sub _inline_lexical_writer : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	my $get    = $me->_inline_lexical_access(@_);
	my $coerce = $me->_inline_lexical_type_coercion('$_[1]', @_);
	
	if ($coerce eq '$_[1]')  # i.e. no coercion
	{
		if (!$opts->{trigger} and !$opts->{weak_ref})
		{
			return sprintf(
				'%s = %s',
				$get,
				$me->_inline_lexical_type_assertion('$_[1]', @_),
			);
		}
		
		return sprintf(
			'%s; %s; %s = $_[1]; %s; %s',
			$me->_inline_lexical_type_assertion('$_[1]', @_),
			$me->_inline_lexical_trigger('$_[1]', $get, @_),
			$get,
			$me->_inline_lexical_weaken(@_),
			$me->_inline_lexical_get(@_),
		);
	}
	
	sprintf(
		'my $val = %s; %s; %s; %s = $val; %s; $val',
		$coerce,
		$me->_inline_lexical_type_assertion('$val', @_),
		$me->_inline_lexical_trigger('$val', $get, @_),
		$get,
		$me->_inline_lexical_weaken(@_),
	);
}

sub _lexical_accessor : method
{
	my $me = shift;
	
	$me->_inline_to_coderef(
		accessor => $me->_inline_lexical_accessor(@_),
		@_,
	);
}

sub _inline_lexical_accessor : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	my $get    = $me->_inline_lexical_access(@_);
	my $coerce = $me->_inline_lexical_type_coercion('$_[1]', @_);
	
	if ($coerce eq '$_[1]')  # i.e. no coercion
	{
		if (!$opts->{trigger} and !$opts->{weak_ref})
		{
			return sprintf(
				'(@_ > 1) ? (%s = %s) : %s',
				$get,
				$me->_inline_lexical_type_assertion('$_[1]', @_),
				$get,
			);
		}
		
		return sprintf(
			'if (@_ > 1) { %s; %s; %s = $_[1]; %s }; %s',
			$me->_inline_lexical_type_assertion('$_[1]', @_),
			$me->_inline_lexical_trigger('$_[1]', $get, @_),
			$get,
			$me->_inline_lexical_weaken(@_),
			$me->_inline_lexical_get(@_),
		);
	}
	
	sprintf(
		'if (@_ > 1) { my $val = %s; %s; %s; %s = $val; %s }; %s',
		$coerce,
		$me->_inline_lexical_type_assertion('$val', @_),
		$me->_inline_lexical_trigger('$val', $get, @_),
		$get,
		$me->_inline_lexical_weaken(@_),
		$me->_inline_lexical_get(@_),
	);
}

sub _inline_lexical_type_coercion : method
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

sub _inline_lexical_type_assertion : method
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

sub _inline_lexical_weaken : method
{
	my $me = shift;
	my ($name, $uniq, $opts) = @_;
	
	return '' unless $opts->{weak_ref};
	
	sprintf(
		q[ Scalar::Util::weaken(%s) if ref(%s) ],
		$me->_inline_lexical_access(@_),
		$me->_inline_lexical_access(@_),
	);
}

sub _inline_lexical_trigger : method
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

Lexical::Accessor - true private attributes for Moose/Moo/Mouse

=head1 SYNOPSIS

   my $accessor = lexical_has identifier => (
      is       => 'rw',
      isa      => Int,
      default  => sub { 0 },
   );
   
   # or...
   lexical_has identifier => (
      is       => 'rw',
      isa      => Int,
      default  => sub { 0 },
      accessor => \$accessor,
   );
   
   # later...
   say $self->$accessor;     # says 0
   $self->$accessor( 1 );    # setter
   say $self->$accessor;     # says 1

=head1 DESCRIPTION

Lexical::Accessor generates coderefs which can be used as methods to
access private attributes for objects.

The private attributes are stored inside-out, and do not add any
accessors to the class' namespace, so are completely invisible to any
outside code, including any subclasses. This gives your attribute
complete privacy: subclasses can define a private (or even public)
attribute with the same name as your private one and they will not
interfere with each other.

Private attributes can not be initialized by L<Moose>/L<Moo>/L<Mouse>
constructors, but you can safely initialize them inside a C<BUILD> sub.

=head2 Functions

=over

=item C<< lexical_has $name?, %options >>

This module exports a function L<lexical_has> which acts much like
Moose's C<has> function, but sets up a private (lexical) attribute
instead of a public one.

Because lexical attributes are stored inside-out, the C<$name> is
completely optional; however a name is recommended because it allows
better error messages to be generated.

The L<lexical_has> function supports the following options:

=over

=item C<< is >>

Moose/Mouse/Moo-style C<ro>, C<rw>, C<rwp> and C<lazy> values are
supported. These control what sort of coderef is returned by the
C<lexical_has> function itself.

   my $reader            = lexical_has "foo" => (is => "ro");
   my $accessor          = lexical_has "foo" => (is => "rw");
   my ($reader, $writer) = lexical_has "foo" => (is => "rwp");

If generating more than one method it is probably clearer to pass in
scalar references to the C<reader>, C<writer>, etc methods, rather than
relying on the return value of the C<lexical_has> function.

=item C<< reader >>, C<< writer >>, C<< accessor >>, C<< predicate >>,
C<< clearer >>

These accept scalar references. The relevant coderefs will be plonked
into them:

   my ($get_foo, $set_foo);
   
   lexical_has foo => (
      reader      => \$get_foo,
      writer      => \$set_foo,
   );

=item C<< default >>, C<< builder >>, C<< lazy >>

Lazy defaults and builders are allowed. Eager (non-lazy) defaults and
builders are currently disallowed. (Use a C<BUILD> sub to set private
attribute values at object construction time.)

The default may be either a non-reference value, or a coderef which
will be called as a method to return the value.

Builders probably make less sense than defaults because they require
a method in the class' namespace. The builder may be a method name, or
the special value C<< '1' >> which will be interpreted as meaning the
attribute name prefixed by "_build_". If a coderef is provided, this is
automatically installed into the class' namespace with the "_build_"
prefix. (This last feature requires L<Sub::Name>.)

=item C<< isa >>

A type constraint for the attribute. L<Moo>-style coderefs are
accepted (including those generated by L<MooX::Types::MooseLike>),
as are L<Moose::Meta::TypeConstraint>/L<MooseX::Types> objects,
and L<Mouse::Meta::TypeConstraint>/L<MouseX::Types> objects, and
of course L<Type::Tiny> type constraints.

String type constraints may also be accepted, but only if
L<Type::Utils> is installed. (String type constraints are reified
using C<dwim_type>.)

=item C<< does >>

As an alternative to C<isa>, you can provide a role name in the
C<does> option.

=item C<< coerce >>

A coderef or L<Type::Coercion> object is accepted.

If the special value C<< '1' >> is provided, the type constraint object
is consulted to find the coercion. (This doesn't work for coderef type
constraints.)

=item C<< trigger >>

A method name or coderef to trigger when a new value is set.

=item C<< auto_deref >>

Boolean indicating whether to automatically dereference array and hash
values if called in list context.

=item C<< init_arg >>

Must be C<undef> if provided at all.

=item C<< required >>

Must be false if provided at all.

=item C<< weak_ref >>

Boolean. Makes the setter weaken any references it is called with.

=item C<< handles >>

Delegates methods. Has slightly different syntax to Moose's option of
the same name - is required to be an arrayref of pairs such that each
pair is a scalar ref followed by a method name, a coderef, or an
arrayref (where the first element is a method name or coderef and
subsequent elements are curried arguments).
 
   my ($get, $post);
  
   lexical_has ua => (
      isa      => 'HTTP::Tiny',
      default  => sub { 'HTTP::Tiny'->new },
      handles  => [
         \$get   => 'get',
         \$post  => 'post',
      ],
   );
   
   # later...
   my $response = $self->$get('http://example.net/');

=item C<< initializer >>, C<< traits >>, C<< lazy_build >>

Not currently implemented. Providing any of these options throws an
error.

=item C<< documentation >>, C<< definition_context >>

Don't do anything, but are allowed; effectively inline comments.

=back

=item C<< HAS_SUB_NAME >>

Indicates the availability of L<Sub::Name> which Lexical::Accessor will
use if available to name coderefs for the benefit of stack traces.

It's also used to name any C<builder> subs which are automatically
installed into your class' namespace.

This function is not exported.

=back

=head2 Class Methods

=over

=item C<< lexical_has >>

This function may also be called as a class method.

=back

=head2 Comparison (benchmarking, etc)

Lexical::Accessor is almost three times faster than
L<MooX::PrivateAttributes>, and almost twenty time faster than
L<MooseX::Privacy>. I'd also argue that it's a more "correct"
implementation of private accessors as (short of performing impressive
L<PadWalker> manipulations), the accessors generated by this module
are completely invisible to subclasses, method dispatch, etc.

Compared to the usual Moose convention of using a leading underscore
to indicate a private method (which is a very loose convention; it is
quite common for subclasses to override such methods!),
L<Lexical::Accessor> clearly offers much better method privacy. There
should be little performance hit from using lexical accessors compared
to normal L<Moose> accessors. (However they are nowhere near the speed
of the XS-powered accessors that L<Moo> I<sometimes> uses and L<Mouse>
I<usually> uses.)

See also: C<< examples/benchmark.pl >> bundled with this release.

=head1 BUGS

Please report any bugs to
L<http://rt.cpan.org/Dist/Display.html?Queue=Lexical-Accessor>.

=head1 SUPPORT

B<< IRC: >> support is available through in the I<< #moops >> channel
on L<irc.perl.org|http://www.irc.perl.org/channels.html>.

=head1 SEE ALSO

L<MooX::PrivateAttributes>,
L<MooX::ProtectedAttributes>,
L<MooseX::Privacy>,
L<Sub::Private>,
L<Method::Lexical>,
etc...

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2013 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=head1 DISCLAIMER OF WARRANTIES

THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

