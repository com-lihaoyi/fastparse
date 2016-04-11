package cssparse

// According to https://www.w3.org/TR/css-syntax-3/

object Ast {

  abstract class ComponentValue

  abstract class SimpleToken extends ComponentValue

  case class IdentToken(name: String) extends SimpleToken

  case class FunctionToken(name: String)

  case class AtWordToken(name: String) extends SimpleToken

  case class HashWordToken(name: String) extends SimpleToken

  case class StringToken(string: String) extends SimpleToken

  case class UrlToken(url: String) extends SimpleToken

  case class NumberToken(number: String) extends SimpleToken

  case class DimensionToken(number: String, dimensionName: String) extends SimpleToken

  case class PercentageToken(number: String) extends SimpleToken

  case class UnicodeRangeToken(left: String, right: String) extends SimpleToken

  case class IncludeMatchToken() extends SimpleToken
  case class DashMatchToken() extends SimpleToken
  case class PrefixMatchToken() extends SimpleToken
  case class SuffixMatchToken() extends SimpleToken
  case class SubstringMatchToken() extends SimpleToken
  case class ColumnToken() extends SimpleToken

  abstract class CToken extends SimpleToken
  case class CdoToken() extends CToken
  case class CdcToken() extends CToken

  case class DelimToken(delimeter: String) extends SimpleToken


  class Block(val leftBracket: String, val rightBracket: String, val values: Seq[ComponentValue]) extends ComponentValue
  case class BracketsBlock(override val values: Seq[ComponentValue]) extends Block("(", ")", values)
  case class CurlyBracketsBlock(override val values: Seq[ComponentValue]) extends Block("{", "}", values)
  case class SquareBracketsBlock(override val values: Seq[ComponentValue]) extends Block("[", "]", values)

  case class FunctionBlock(name: String, values: Seq[ComponentValue]) extends ComponentValue

  abstract class Rule

  case class QualifiedRule(selector: Seq[ComponentValue], block: CurlyBracketsBlock) extends Rule

  case class AtRule(name: String, selector: Seq[ComponentValue], block: Option[CurlyBracketsBlock]) extends Rule

  case class RuleList(rules: Seq[Rule])

  case class Stylesheet(rules: Seq[Either[Rule, CToken]])

  case class Declaration(name: String, value: Seq[ComponentValue], isImportant: Boolean)

  case class DeclarationList(declarations: Seq[Either[Declaration, AtRule]])
}
