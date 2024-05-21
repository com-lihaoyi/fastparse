package cssparse

import utest._
import fastparse._
import Ast._

object CssTests extends TestSuite {
  val tests = this {
    test("basic"){
      test("test1"){
        val Parsed.Success(value1, index1) = parse(
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
          """.stripMargin,
          CssRulesParser.ruleList(_)
        ) : @unchecked

        assert(
          value1 ==
            RuleList(Seq(
              QualifiedRule(Left(
                MultipleSelector(
                  ElementSelector("p"),
                  Seq((">", ElementSelector("a"))))),
                DeclarationList(Seq(
                  Left(Declaration("color", Seq(IdentToken("blue")), false)),
                  Left(Declaration("text-decoration", Seq(IdentToken("underline")), false))))))),
          index1 == 120
        )
      }

      test("test2"){
        val Parsed.Success(value2, index2) = parse(
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
          """.stripMargin, CssRulesParser.ruleList(_)) : @unchecked

        assert(
          value2 ==
            RuleList(Seq(
              QualifiedRule(
                Left(ElementSelector("html")),
                DeclarationList(Seq(
                  Left(Declaration("font-family", Seq(IdentToken("sans-serif")), false)),
                  Left(Declaration("-webkit-text-size-adjust", Seq(PercentageToken("100")), false)),
                  Left(Declaration("-ms-text-size-adjust", Seq(PercentageToken("100")), false))))))),
          index2 == 363
        )
      }

      test("test3"){
        val Parsed.Success(value3, index3) = parse(
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
          """.stripMargin, CssRulesParser.ruleList(_)) : @unchecked

        val expected = RuleList(Seq(
          QualifiedRule(
            Left(ElementSelector("after")),
            DeclarationList(Seq(
              Left(Declaration("color", Seq(HashWordToken("000")), true)),
              Left(Declaration("text-shadow", Seq(IdentToken("none")), true)),
              Left(Declaration("background", Seq(IdentToken("transparent")), true)),
              Left(Declaration("-webkit-box-shadow", Seq(IdentToken("none")), true)),
              Left(Declaration("box-shadow", Seq(IdentToken("none")), true)))))))
        assert(
          value3 == expected,
          index3 == 239
        )
      }

      test("test4"){

        val Parsed.Success(value4, index4) = parse(
          """
            |
            | .label-info[href]:hover,
            | .label-info[href]:focus {
            |          background-color: #31b0d5;
            |  }
            |
          """.stripMargin, CssRulesParser.ruleList(_)) : @unchecked

        assert(
          value4 ==
            RuleList(Seq(
              QualifiedRule(Left(
                MultipleSelector(
                  ComplexSelector(None, Seq(
                    ClassSelectorPart(AttributeSelector(Some("label-info"), Seq(("href", None, None)))),
                    PseudoSelectorPart(":hover", Seq()))),
                  Seq((",", ComplexSelector(None, Seq(
                    ClassSelectorPart(AttributeSelector(Some("label-info"), Seq(("href", None, None)))),
                    PseudoSelectorPart(":focus", Seq()))))))),
                DeclarationList(Seq(Left(
                  Declaration("background-color", Seq(HashWordToken("31b0d5")), false))))))),
          index4 == 107
        )
      }

      test("test5"){
        val Parsed.Success(value5, _) = parse(
          """
            |
            |   [hidden],
            |   template {
            |     display: none;
            |   }
            |
          """.stripMargin, CssRulesParser.ruleList(_)) : @unchecked

        assert(value5 == RuleList(Seq(
          QualifiedRule(
            Left(MultipleSelector(
              AttributeSelector(None, Seq(("hidden", None, None))),
              Seq((",", ElementSelector("template"))))),
            DeclarationList(Seq(
              Left(Declaration("display", Seq(IdentToken("none")), false))))))))
      }

      test("test6"){
        val Parsed.Success(value6, _) = parse(
          """
            |
            |@media (min-width: 768px) {
            |          .lead {
            |            font-size: 21px;
            |          }
            |        }
            |
          """.stripMargin, CssRulesParser.ruleList(_)) : @unchecked

        assert(value6 == RuleList(Seq(
          AtRule("media", Seq(
            BracketsBlock(Seq(IdentToken("min-width"), DelimToken(":"), DimensionToken("768", "px")))),
            Some(Right(RuleList(Seq(
              QualifiedRule(Left(
                ComplexSelector(None, Seq(ClassSelectorPart(ElementSelector("lead"))))),
                DeclarationList(Seq(Left(
                  Declaration("font-size", Seq(DimensionToken("21", "px")), false)))))))))))))
      }

      test("test7"){
        val Parsed.Success(value7, _) = parse(
          """|
             |@rule {
             |        unicode-range: U+26;                 /* single codepoint */
             |        unicode-range: U+0-7F;
             |        unicode-range: U+0025-00FF;          /* codepoint range */
             |        unicode-range: U+4??;                /* wildcard range */
             |        unicode-range: U+0025-00FF, U+4??;
             |}
          """.stripMargin, CssRulesParser.ruleList(_)) : @unchecked
        assert(value7 == RuleList(Seq(
          AtRule("rule", Seq(), Some(Left(
            DeclarationList(Seq(
              Left(Declaration("unicode-range", Seq(UnicodeRangeToken("26", "26")), false)),
              Left(Declaration("unicode-range", Seq(UnicodeRangeToken("0", "7F")), false)),
              Left(Declaration("unicode-range", Seq(UnicodeRangeToken("0025", "00FF")), false)),
              Left(Declaration("unicode-range", Seq(UnicodeRangeToken("4??", "4??")), false)),
              Left(Declaration("unicode-range", Seq(
                UnicodeRangeToken("0025", "00FF"), DelimToken(","), UnicodeRangeToken("4??", "4??")), false))))))))))

      }

      // https://github.com/com-lihaoyi/fastparse/issues/255
      test("issue-#255: comments at the end of a block"){
        val Parsed.Success(value2, index2) = parse(
          """
            |p {
            |  font-family: sans-serif;
            |  color: red;
            |  /* test comment */
            |}
            |
          """.stripMargin, CssRulesParser.ruleList(_)) : @unchecked

        assert(
          value2 ==
            RuleList(Seq(
              QualifiedRule(
                Left(ElementSelector("p")),
                DeclarationList(Seq(
                  Left(Declaration("font-family", Seq(IdentToken("sans-serif")), false)),
                  Left(Declaration("color", Seq(IdentToken("red")), false))))))),
          index2 == 80
        )
      }
    }
  }
}
