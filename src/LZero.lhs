<h1> The LZero language </h1>

> module LZero where
Variables are just strings.
> data V = V String

As are constants.
> data C = C String

Structures consist of a constant and a list of subterms.
> data S = S C [S]
Thus, a consant is a special case of a structure with arity `0`.
