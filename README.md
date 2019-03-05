[Warren (Link to literate file)](https://bollu.github.io/warren/)
=========================================

An implementation of the 
[warren abstract machine](http://wambook.sourceforge.net/wambook.pdf),
as a learning exercise in Haskell parallelism.

I wish to use [deltas](http://github.com/bollu/paper-deltas) for 
incremental and parallel computation of datalog/prolog queries. That should
be a very interesting expriment of the `deltas` infrastructure for an
abstract machine such as `Warren`.

## Reading material
- [The warren abstract machine, A tutorial reconstruction](http://wambook.sourceforge.net/wambook.pdf)
- [Datalog lecture notes](https://www.cs.cmu.edu/~fp/courses/lp/lectures/26-datalog.pdf)
- [Logic programming course@CMU](https://www.cs.cmu.edu/~fp/courses/lp/)
## Building from source

Recommend setting up the `post-commit` hook so the docs auto-build:

```
$ (at root of repo)
$ cp post-commit .git/hooks
```

To build, simply use `cabal`:
```
cabal v2-build
```
