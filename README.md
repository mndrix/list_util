# Synopsis

    :- use_module(list_util, [take/3]).
    ?- take(2, [hello, world, etc], Words).
    Words = [hello, world].

# Description

Prolog code works with lots of lists.  This is a collection of predicates
that I've found useful from time to time across projects.

Some of these predicates are inspired by functions in Haskell's
[Data.List](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html)
library. Prolog's reversible predicates allow us to get by with
fewer definitions than Haskell needs.

See `list_util.pl` documentation below for details about each exported
predicate.

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(list_util).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/list_util
