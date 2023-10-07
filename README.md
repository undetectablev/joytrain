#JoyTrain

JoyTrain is a pseudorandom algebraic multiquine relay written in a subset of [Joy](https://www.kevinalbrecht.com/code/joy-mirror/joy.html) programming language by [Manfred von Thun](http://fogus.me/important/von-thun/). It works by specializing a probabilistic pattern matcher with a non-confluent TRS and applying the residual program to itself. All ad-hoc specialization and interpretation is done in [Chez Scheme](https://cisco.github.io/ChezScheme/).

##How to run:

```
chezscheme --script joy.scm joytrain.joy > 1.joy
chezscheme --script joy.scm 1.joy > 2.joy
chezscheme --script joy.scm 2.joy > 3.joy
...
```

##Building from sources:

```
chezscheme --script src/joytrain.scm > joytrain.joy
```

##Notes

* All recursion is eliminated by the fixpoint combinator.
* Quotations `[...]` are treated as extensional, so no axioms violating extensionality are used.
* Strings `"..."` are intensional. They are used with the `intern` combinator as patterns to avoid their further overwriting.
* No type checking is implemented, so no axioms like `id == swap swap` are used.
* Strategy is the dumbest possible:
  1. randomly apply axioms N times
  2. normalize
  3. randomly apply axioms once
* Doesn't work with the [original Joy interpreter](https://github.com/Wodan58/Joy) becouse it crashes with segfault for some reason.
