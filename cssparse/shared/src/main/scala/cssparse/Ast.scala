package cssparse

// According to https://www.w3.org/TR/css-syntax-3/

object Ast {

  sealed abstract class ComponentValue

  sealed abstract class SimpleToken extends ComponentValue

  sealed case class IdentToken(name: String) extends SimpleToken
  sealed case class FunctionToken(name: String)
  sealed case class AtWordToken(name: String) extends SimpleToken
  sealed case class HashWordToken(name: String) extends SimpleToken
  sealed case class StringToken(string: String) extends SimpleToken
  sealed case class UrlToken(url: String) extends SimpleToken
  sealed case class NumberToken(number: String) extends SimpleToken
  sealed case class DimensionToken(number: String, dimensionName: String) extends SimpleToken
  sealed case class PercentageToken(number: String) extends SimpleToken
  sealed case class UnicodeRangeToken(left: String, right: String) extends SimpleToken

  sealed case class IncludeMatchToken() extends SimpleToken
  sealed case class DashMatchToken() extends SimpleToken
  sealed case class PrefixMatchToken() extends SimpleToken
  sealed case class SuffixMatchToken() extends SimpleToken
  sealed case class SubstringMatchToken() extends SimpleToken
  sealed case class ColumnToken() extends SimpleToken

  sealed abstract class CToken extends SimpleToken
  sealed case class CdoToken() extends CToken
  sealed case class CdcToken() extends CToken

  sealed case class DelimToken(delimeter: String) extends SimpleToken

  sealed class Block(val leftBracket: String, val rightBracket: String, val values: Seq[ComponentValue])
    extends ComponentValue
  sealed case class BracketsBlock(override val values: Seq[ComponentValue]) extends Block("(", ")", values)
  sealed case class CurlyBracketsBlock(override val values: Seq[ComponentValue]) extends Block("{", "}", values)
  sealed case class SquareBracketsBlock(override val values: Seq[ComponentValue]) extends Block("[", "]", values)

  sealed case class FunctionBlock(name: String, bracketsBlock: BracketsBlock) extends ComponentValue

  sealed abstract class Rule

  sealed case class QualifiedRule(selector: Seq[ComponentValue], block: CurlyBracketsBlock) extends Rule
  sealed case class AtRule(name: String, selector: Seq[ComponentValue], block: Option[CurlyBracketsBlock]) extends Rule
  sealed case class RuleList(rules: Seq[Rule])

  sealed case class Stylesheet(rules: Seq[Either[Rule, CToken]])

  sealed case class Declaration(name: String, value: Seq[ComponentValue], isImportant: Boolean)
  sealed case class DeclarationList(declarations: Seq[Either[Declaration, AtRule]])
}
