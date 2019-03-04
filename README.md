Warren
======

An implementation of the 
[warren abstract machine](http://wambook.sourceforge.net/wambook.pdf),
as a learning exercise in Haskell parallelism.

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
