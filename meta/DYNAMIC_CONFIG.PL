if ($] < 5.010000) {
	$meta->{prereqs}{runtime}{requires}{'Hash::Util::FieldHash::Compat'} = 0;
}

if (eval { require Sub::HandlesVia; 1 }) {
	$meta->{prereqs}{runtime}{requires}{'Sub::HandlesVia'} = '0.025';
}
