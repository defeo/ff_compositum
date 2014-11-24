# Fast arithmetic for the algebraic closure of finite fields

**Authors:** [Luca De Feo](http://defeo.lu), Javad Doliskani
and [Éric Schost](http://www.csd.uwo.ca/~eschost/)

>  We present algorithms to construct and do arithmetic operations in
>  the algebraic closure of the finite field GF(p). Our
>  approach is inspired by algorithms for constructing irreducible
>  polynomials, which first reduce to prime power degrees, then use
>  composita techniques. We use similar ideas to give efficient
>  algorithms for embeddings and isomorphisms.

**Notice:** this is the authors' version of the work presented at the
[International Symposium on Symbolic and Algebraic Computation 2014](http://www.issac-conference.org/2014/)
in Kobe. It is posted here by permission of ACM for your personal use,
which arguably includes your right to fork this repository. A
proceedings-frozen version was published in the *Proceedings of the
39th International Symposium on Symbolic and Algebraic Computation*,
pages 122-129, <http://dx.doi.org/10.1145/2608628.2608672>, but the
paper keeps evolving!

This repository contains the [LaTeX](http://www.latex-project.org/)
source code of the paper, the code used for testing purposes, and some
more C code giving a proof-of-concept.  The
[Sage](http://www.sagemath.org/) tests must be run against the
experimental branch hosted at
<https://github.com/defeo/sage/tree/ff_compositum>.
