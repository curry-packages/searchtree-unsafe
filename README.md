searchtree-unsafe: Representing non-deterministic computations as a search tree
===============================================================================

This package contains the library `Control.Search.Unsafe`
which defines a representation of a search space as a tree
and various search strategies on this tree.
The library implements **strong encapsulation** as discussed in the
[JFLP'04 paper](http://danae.uni-muenster.de/lehre/kuchen/JFLP/articles/2004/S04-01/A2004-06/JFLP-A2004-06.pdf).

In contrast to the library `Control.SearchTree` of package `searchtree`,
the implementation of this library has the property that free variables
that are not bound in the encapsulated expression remain free!
This may lead to non-determinism if such an escaped
variable is bound later via pattern matching.

--------------------------------------------------------------------------
