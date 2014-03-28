=pod

=encoding utf-8

=head1 PURPOSE

Test that Lexical::Accessor can be subclassed to enable other
interesting accessor generators to build upon it.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2014 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.


=cut

use strict;
use warnings;
use Test::More;

BEGIN {
	package Local::MyAccessor;
	
	use base qw(Lexical::Accessor);
	our @EXPORT = qw( has );
	$INC{'Local/MyAccessor.pm'} = __FILE__;
	
	*_generate_has = __PACKAGE__->can('_generate_lexical_has');
	
	# Install as a normal method
	sub _install_coderef {
		my $me = shift;
		my ($target, $coderef, undef, undef, $opts) = @_;
		if (!ref $target and $target =~ /\A[^\W0-9]\w+\z/) {
			no strict qw(refs);
			my $name = "$opts->{package}\::$target";
			*{$name} = $coderef;
			return;
		}
		$me->SUPER::_install_coderef(@_);
	}
	
	# Store in a hashref instead of inside-out.
	sub _inline_lexical_access {
		my $me = shift;
		my ($name, $uniq, $opts) = @_;
		sprintf(
			q[ $_[0]{%s} ],
			$name,
		);
	}
};

BEGIN {
	package Local::MyClass;
	use Local::MyAccessor;
	
	sub new {
		bless {}, shift;
	}
	
	has foo => (accessor => 'foo');
	has bar => (accessor => 'bar');
};

my $obj = Local::MyClass->new;

$obj->foo(42);
$obj->bar(666);

is($obj->foo, 42);
is($obj->bar, 666);

is_deeply(
	+{ %$obj },
	+{ foo => 42, bar => 666 },
);

done_testing;
