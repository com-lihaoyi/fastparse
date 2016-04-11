package cssparse

import utest._
import fastparse.all._
import Ast._

import scala.collection.mutable.ArrayBuffer

object CssTests extends TestSuite {
  val tests = this {
    'basic {
      'test1 {
        val Parsed.Success(value1, index1) = CssParser.stylesheet.parse(
          """
            |
            |p > a {
            |    color: blue;
            |    text-decoration: underline;
            |  }
            |
          """.stripMargin)

        assert(
          value1 ==
            Stylesheet(ArrayBuffer(
              Left(QualifiedRule(ArrayBuffer(
                  IdentToken("p"),
                  DelimToken(">"),
                  IdentToken("a")),
                CurlyBracketsBlock(ArrayBuffer(
                  IdentToken("color"),
                  DelimToken(":"),
                  IdentToken("blue"),
                  DelimToken(";"),
                  IdentToken("text-decoration"),
                  DelimToken(":"),
                  IdentToken("underline"),
                  DelimToken(";"))))))),
          index1 == 74
        )
      }

      'test2 {
        val Parsed.Success(value2, index2) = CssParser.stylesheet.parse(
          """
            |
            |/*!
            | * Bootstrap v3.3.6 (http://getbootstrap.com)
            | * Copyright 2011-2016 Twitter, Inc.
            | * Licensed under MIT (https://github.com/twbs/bootstrap/blob/master/LICENSE)
            | */
            |/*! normalize.css v3.0.3 | MIT License | github.com/necolas/normalize.css */
            |html {
            |  font-family: sans-serif;
            |  -webkit-text-size-adjust: 100%;
            |      -ms-text-size-adjust: 100%;
            |}
            |
          """.stripMargin)

        assert(
          value2 ==
            Stylesheet(ArrayBuffer(
              Left(QualifiedRule(ArrayBuffer(
                IdentToken("html")),
                CurlyBracketsBlock(ArrayBuffer(
                  IdentToken("font-family"),
                  DelimToken(":"),
                  IdentToken("sans-serif"),
                  DelimToken(";"),

                  IdentToken("-webkit-text-size-adjust"),
                  DelimToken(":"),
                  PercentageToken("100"),
                  DelimToken(";"),

                  IdentToken("-ms-text-size-adjust"),
                  DelimToken(":"),
                  PercentageToken("100"),
                  DelimToken(";"))))))),
          index2 == 363
        )
      }

      'test3 {
        val Parsed.Success(value3, index3) = CssParser.stylesheet.parse(
          """
            |
            |  after {
            |          color: #000 !important;
            |          text-shadow: none !important;
            |          background: transparent !important;
            |          -webkit-box-shadow: none !important;
            |          box-shadow: none !important;
            |        }
            |
          """.stripMargin)

        assert(
          value3 ==
            Stylesheet(ArrayBuffer(
              Left(QualifiedRule(ArrayBuffer(
                IdentToken("after")),
                CurlyBracketsBlock(ArrayBuffer(
                  IdentToken("color"),
                  DelimToken(":"),
                  HashWordToken("000"),
                  DelimToken("!"),
                  IdentToken("important"),
                  DelimToken(";"),

                  IdentToken("text-shadow"),
                  DelimToken(":"),
                  IdentToken("none"),
                  DelimToken("!"),
                  IdentToken("important"),
                  DelimToken(";"),

                  IdentToken("background"),
                  DelimToken(":"),
                  IdentToken("transparent"),
                  DelimToken("!"),
                  IdentToken("important"),
                  DelimToken(";"),

                  IdentToken("-webkit-box-shadow"),
                  DelimToken(":"),
                  IdentToken("none"),
                  DelimToken("!"),
                  IdentToken("important"),
                  DelimToken(";"),

                  IdentToken("box-shadow"),
                  DelimToken(":"),
                  IdentToken("none"),
                  DelimToken("!"),
                  IdentToken("important"),
                  DelimToken(";"))))))),
          index3 == 239
        )

      }
    }
  }
}
