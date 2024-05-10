FastParse [![Build Status](https://travis-ci.org/lihaoyi/fastparse.svg?branch=master)](https://travis-ci.org/lihaoyi/fastparse) [![Join the chat at https://gitter.im/lihaoyi/Ammonite](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/lihaoyi/fastparse?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
=========

This is where the code for the FastParse parsing library lives! If you want
to use Fastparse, you probably will want to check out the documentation:

- [Documentation](https://com-lihaoyi.github.io/fastparse)

If you use FastParse and like it, you will probably enjoy the following book by the Author:

- [*Hands-on Scala Programming*](https://www.handsonscala.com/)

*Hands-on Scala* has uses FastParse extensively throughout the book, with
the entirety of *Chapter 19: Parsing Structured Text* dedicated to 
the library and *Chapter 20: Implementing a Programming Language* making heavy
use of it. *Hands-on Scala* is a great way to level up your skills in Scala
in general and FastParse in particular.

For a good hands-on tutorial working through the basics of how to use this
library, check out the following blog post:

- [Easy Parsing with Parser Combinators](http://www.lihaoyi.com/post/EasyParsingwithParserCombinators.html)

This readme contains some developer docs, if you intend on working on the
fastparse repo, not just using it as a library.

Developer Docs
==============

The core of FastParse lives in the `fastparse/` folder. It is cross-built
ScalaJVM/Scala.js codebase, with almost everything shared between the two
platforms in the `fastparse/src/` and minor differences in `fastparse/src-js/`
and `fastparse/src-jvm/`.

The three subprojects `scalaparse/`, `pythonparse/` and `cssparse/` are
FastParse parsers for those respective languages. These are both usable as
standalone libraries, and also serve as extensive test-suites and use-cases for
FastParse itself. Each of those projects clones & parses large quantities of
code from Github as part of *their* own test suites.

`perftests/` constains performance tests for main projects in the library
including `ScalaParse`, `PythonParse`, `CssParse`, `readme/` contains the
documentation site, which contains several live demos of FastParse parsers
compiled to Scala.js. These all live in `demo/`.

Common Commands
---------------

Note: you should use mill 0.11 or later.

- `mill -w "fastparse.jvm[2.12.10].test"` runs the main testsuite. If you're
  hacking on FastParse, this is often where you want to go

- You can run the other suites via `fastparse.js`, `scalaparse.jvm`, etc. if you
  wish, but I typically don't and leave that to CI unless I'm actively working
  on the sub-project

- You can use `mill -w "fastparse.jvm[_].test"` to run it under different Scala
  versions, but again I usually don't bother

- `mill __.test.test` is the aggregate test-all command, but is pretty slow. You
  can use `mill "__.jvm[2.12.17].test"` to run all tests only under JVM/Scala-2.12,
  which is much faster and catches most issues

- `mill demo.fullOpt && sbt readme/run` builds the documentation site, which can
  then be found at `readme/target/scalatex/index.html`

Contribution Guidelines
-----------------------

- **If you're not sure if something is a bug or not, ask on Gitter first =)**
- **All code PRs should come with**: a meaningful description, inline comments for important things, unit tests, and a green build
- **Non-trivial changes, including bug fixes, should appear in the changelog**. Feel free to add your name and link to your github profile!
- **New features should be added to the relevant parts of the documentation**
- **To a large extent, FastParse is designed so that you can extend it in your own code** without needing to modify the core. If you want to add features, be prepared to argue why it should be built-in and not just part of your own code.
- **It's entirely possible your changes won't be merged**, or will get ripped out later. This is also the case for my changes, as the Author!
- **Even a rejected/reverted PR is valuable**! It helps explore the solution space, and know what works and what doesn't. For every line in the repo, at least three lines were tried, committed, and reverted/refactored, and more than 10 were tried without committing.
- **Feel free to send Proof-Of-Concept PRs** that you don't intend to get merged.
- **No binary or source compatibility is guaranteed between any releases**. FastParse keeps compatibility following the SemVer rule, upgrading is usually trivial, and I don't expect existing functionality to go away

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
