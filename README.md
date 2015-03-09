scala-parser
============

```scala
"com.lihaoyi" %% "scala-parser" % "0.1.2"
```

scala-parser is a [parboiled2](https://github.com/sirthias/parboiled2) parser for the [Scala programming language](https://github.com/scala/scala). It aims to parse Scala code using the parboiled2 library in a much simpler way than Scala's existing hand-rolled parser.

scala-parser currently parses the vast majority of Scala code. Not supported syntax includes:

- Unicode Escapes

However, due to the nature of the parboiled2 library, adding support for either of these things would be pretty straightforward, if tedious.

scala-parser currently has a small unit test suite to prevent regressions of already-fixed bugs in the parser, as well as a more thorough "parse everything" testsuite that runs against all `.scala` files in the following projects:

- https://github.com/scala/scala
- https://github.com/scala-js/scala-js
- https://github.com/akka/akka
- https://github.com/scalaz/scalaz
- https://github.com/milessabin/shapeless
- https://github.com/lift/framework
- https://github.com/playframework/playframework

It parses everything in all of these projects, modulo a blacklist which you can see in the test code which mainly excludes unsupported functionality (mentioned above) and broken Scala files (scripts, negative-tests, etc.)

To run tests on all these projects, run

```
git submodule init
git submodule update
sbt test
```

scala-parser currently only parses code to identify it and does not construct any AST of any sort. However, doing so wouldn't be difficult to implement on top of the existing relatively-straightforward parser implementation.

scala-parser is still a work-in-progress: performance has not been measured, refactorings can still be done, and it still can be made to do more useful things than it currently does (e.g. constructing an AST). However, the fact that it is able to parse all the supported files in the above repositories speaks for its completeness and correctness.

scala-parser originally was a fork of https://github.com/rcano/sps, but has changed so much that it is completely unrecognizable.

License
=======

The MIT License (MIT)

Copyright (c) 2014 Li Haoyi (haoyi.sg@gmail.com)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
