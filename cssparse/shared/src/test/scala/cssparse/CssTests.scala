package cssparse

import utest._
import fastparse.all._
import Ast._

import scala.collection.mutable.ArrayBuffer

object CssTests extends TestSuite {
  val tests = this {
    'basic {
      'test1 {
        val Parsed.Success(value1, index1) = CssRulesParser.ruleList.parse(
          """
            |
            |  p    >    a
            |  {
            |    color      :         blue;
            |
            |
            |
            |    text-decoration              :       underline
            |  }
            |
          """.stripMargin)

        assert(
          value1 ==
            RuleList(ArrayBuffer(
              QualifiedRule(Left(
                MultipleSelector(
                  ElementSelector("p"),
                  ArrayBuffer((">", ElementSelector("a"))))),
                DeclarationList(ArrayBuffer(
                  Left(Declaration("color", ArrayBuffer(IdentToken("blue")), false)),
                  Left(Declaration("text-decoration", ArrayBuffer(IdentToken("underline")), false))))))),
          index1 == 120
        )
      }

      'test2 {
        val Parsed.Success(value2, index2) = CssRulesParser.ruleList.parse(
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
            RuleList(ArrayBuffer(
              QualifiedRule(
                Left(ElementSelector("html")),
                DeclarationList(ArrayBuffer(
                  Left(Declaration("font-family", ArrayBuffer(IdentToken("sans-serif")), false)),
                  Left(Declaration("-webkit-text-size-adjust", ArrayBuffer(PercentageToken("100")), false)),
                  Left(Declaration("-ms-text-size-adjust", ArrayBuffer(PercentageToken("100")), false))))))),
          index2 == 363
        )
      }

      'test3 {
        val Parsed.Success(value3, index3) = CssRulesParser.ruleList.parse(
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
            RuleList(ArrayBuffer(
              QualifiedRule(
                Left(ElementSelector("after")),
                DeclarationList(ArrayBuffer(
                  Left(Declaration("color", ArrayBuffer(HashWordToken("000")), true)),
                  Left(Declaration("text-shadow", ArrayBuffer(IdentToken("none")), true)),
                  Left(Declaration("background", ArrayBuffer(IdentToken("transparent")), true)),
                  Left(Declaration("-webkit-box-shadow", ArrayBuffer(IdentToken("none")), true)),
                  Left(Declaration("box-shadow", ArrayBuffer(IdentToken("none")), true))))))),
          index3 == 239
        )
      }

      'test4 {

        val Parsed.Success(value4, index4) = CssRulesParser.ruleList.parse(
          """
            |
            | .label-info[href]:hover,
            | .label-info[href]:focus {
            |          background-color: #31b0d5;
            |  }
            |
          """.stripMargin)

        assert(
          value4 ==
            RuleList(ArrayBuffer(
              QualifiedRule(Left(
                MultipleSelector(
                  ComplexSelector(None, ArrayBuffer(
                    ClassSelectorPart(AttributeSelector(Some("label-info"), ArrayBuffer(("href", None, None)))),
                    PseudoSelectorPart(":hover", ArrayBuffer()))),
                  ArrayBuffer((",", ComplexSelector(None, ArrayBuffer(
                    ClassSelectorPart(AttributeSelector(Some("label-info"), ArrayBuffer(("href", None, None)))),
                    PseudoSelectorPart(":focus", ArrayBuffer()))))))),
                DeclarationList(ArrayBuffer(Left(
                  Declaration("background-color", ArrayBuffer(HashWordToken("31b0d5")), false))))))),
          index4 == 107
        )
      }

      'test5 {
        val Parsed.Success(value5, index5) = CssRulesParser.ruleList.parse(
          """
            |
            |   [hidden],
            |   template {
            |     display: none;
            |   }
            |
          """.stripMargin)

        assert(value5 == RuleList(ArrayBuffer(
          QualifiedRule(
            Left(MultipleSelector(
              AttributeSelector(None, ArrayBuffer(("hidden", None, None))),
              ArrayBuffer((",", ElementSelector("template"))))),
            DeclarationList(ArrayBuffer(
              Left(Declaration("display", ArrayBuffer(IdentToken("none")), false))))))))
      }

      'test6 {
        val Parsed.Success(value6, index6) = CssRulesParser.ruleList.parse(
          """
            |
            |@media (min-width: 768px) {
            |          .lead {
            |            font-size: 21px;
            |          }
            |        }
            |
          """.stripMargin)

        assert(value6 == RuleList(ArrayBuffer(
          AtRule("media", ArrayBuffer(
            BracketsBlock(ArrayBuffer(IdentToken("min-width"), DelimToken(":"), DimensionToken("768", "px")))),
            Some(Right(RuleList(ArrayBuffer(
              QualifiedRule(Left(
                ComplexSelector(None, ArrayBuffer(ClassSelectorPart(ElementSelector("lead"))))),
                DeclarationList(ArrayBuffer(Left(
                  Declaration("font-size", ArrayBuffer(DimensionToken("21", "px")), false)))))))))))))
      }

      'test7 {
        val Parsed.Success(value7, index7) = CssRulesParser.ruleList.parse(
          """|
             |@rule {
             |        unicode-range: U+26;                 /* single codepoint */
             |        unicode-range: U+0-7F;
             |        unicode-range: U+0025-00FF;          /* codepoint range */
             |        unicode-range: U+4??;                /* wildcard range */
             |        unicode-range: U+0025-00FF, U+4??;
             |}
          """.stripMargin)
        assert(value7 == RuleList(ArrayBuffer(
          AtRule("rule", ArrayBuffer(), Some(Left(
            DeclarationList(ArrayBuffer(
              Left(Declaration("unicode-range", ArrayBuffer(UnicodeRangeToken("26", "26")), false)),
              Left(Declaration("unicode-range", ArrayBuffer(UnicodeRangeToken("0", "7F")), false)),
              Left(Declaration("unicode-range", ArrayBuffer(UnicodeRangeToken("0025", "00FF")), false)),
              Left(Declaration("unicode-range", ArrayBuffer(UnicodeRangeToken("4??", "4??")), false)),
              Left(Declaration("unicode-range", ArrayBuffer(
                UnicodeRangeToken("0025", "00FF"), DelimToken(","), UnicodeRangeToken("4??", "4??")), false))))))))))

      }
    }
  }
}
