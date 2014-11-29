scala-parser
============

scala-parser is a [parboiled2](https://github.com/sirthias/parboiled2) parser for the [Scala programming language](https://github.com/scala/scala). It aims to parse Scala code using the parboiled2 library in a much simpler way than Scala's existing hand-rolled parser.

scala-parser currently parses the vast majority of Scala code. Not supported syntax includes:

- Unicode Escapes
- XML Literals

However, due to the nature of the parboiled2 library, adding support for either of these things would be pretty straightforward, if tedious.

scala-parser currently has a small unit test suite to prevent regressions of already-fixed bugs in the parser, as well as a more thorough "parse everything" testsuite that runs against all `.scala` files in the following projects:

- https://github.com/scala/scala
- https://github.com/scala-js/scala-js
- https://github.com/akka/akka
- https://github.com/scalaz/scalaz
- https://github.com/milessabin/shapeless

It parses everything in all of these projects, modulo a blacklist which you can see in the test code which mainly excludes unsupported functionality (mentioned above) and broken Scala files (scripts, negative-tests, etc.)

To run tests on all these projects, run

```
git submodule init
git submodule update
sbt test
```

scala-parser currently only parses code to identify it and does not construct any AST of any sort. However, doing so wouldn't be difficult to implement on top of the existing relatively-straightforward parser implementation.

scala-parser is still a work-in-progress: performance has not been measured, refactorings can still be done, and it still can be made to do more useful things than it currently does (e.g. constructing an AST). However, the fact that it is able to parse all the supported files in the above repositories speaks for its completeness and correctness.