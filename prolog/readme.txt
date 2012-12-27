---+ Name

=list_util= - Predicates for working with lists

---+ Synopsis

==
:- use_module(list_util, [take/3]).
main :-
    take([hello, world, etc], 2, Words),
    writeln(Words).
==

---+ Description

Prolog code works with lots of lists.  This is a collection of predicates
that I've found useful from time to time across projects.

Some of these predicates are inspired by functions in Haskell's
[[Data.List][http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html]]
library. Prolog's reversible predicates allow us to get by with
fewer definitions than Haskell needs.

@author Michael Hendricks <michael@ndrix.org>
@license BSD
