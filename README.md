# lambda-pi

An implementation in Scala of lambda-pi, a small dependently-typed
language as described by sections 2 and 3 of the paper,
[A tutorial implementation of a dependently typed lambda calculus](http://www.andres-loeh.de/LambdaPi/)
by Löh, McBride and Swierstra (which presents an implementation in
Haskell).

I implemented λπ (as described by sections 2 and 3
of the paper, but not section 4
("Beyond λπ").
However, I additionally implemented Sigma types,
which are dependent sums and
also called "dependent pairs".

The concrete syntax I used is:

	    Term ::=
		Term : Term  // annotation
		Term => Term // Pi type
		λ Term       // abstraction
		Term Term    // application
		Term × Term  // Sigma type
		<Term, Term> // pair
		. n          // projection (n = 0,1)
		Id           // global name
		# n          // bound var (to nth binder)
		*            // type type
		( Term )     // grouped term
	
