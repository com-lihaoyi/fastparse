package test.fastparse

import utest._
import fastparse._

import scala.language.postfixOps

/**
  * This is a regression test for the pathological behavior (#77) where
  * `traceParsers` grows exponentially in cases with lots of backtracking.
  *
  * Without that issue being fixed, the this test runs forever and never
  * terminates. With that issue fixed, it should complete trivially quickly.
  */
object GnipSubSyntaxTest extends TestSuite {
  object GnipRuleParser {
    import SingleLineWhitespace._

    def keyword[_p: P] = P(CharIn("a-z")!)
    def maybeNegatedKeyword[_p: P] = P((("-"?) ~~ keyword)!)

    def keywordGroupWithoutOrClause[_p: P] = P((maybeNegatedKeyword | (("-"?) ~~ keywordsInParentheses))!)
    def keywordGroup[_p: P] = P(orClause | keywordGroupWithoutOrClause)

    def keywordsInParentheses[_p: P] = P("(" ~ gnipKeywordPhrase ~ ")")
    def orClause[_p: P] = P(!(("-" ~~ keywordGroupWithoutOrClause.rep(1)) ~ "OR") ~ keywordGroupWithoutOrClause ~ ("OR"!) ~ gnipKeywordPhrase)
    def gnipKeywordPhrase[_p: P]: P[String] = P(keywordGroup.rep(1))!

    def parse[_p: P] = P(Start ~ gnipKeywordPhrase ~ End)
  }

  object GnipRuleValidator {

    def apply(rule: String) = parse(rule, GnipRuleParser.parse(_))
  }

  val tests = Tests {
    test("fail"){
      val res = GnipRuleValidator("( ab ( cd ( ef ( gh ( ij ( ( hello ( world ) bla ) lol ) hehe ) ) ) xz )")
      assert(!res.isSuccess)
    }

  }
}
