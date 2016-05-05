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

  sealed abstract class Selector
  sealed abstract class SingleSelector extends Selector
  sealed abstract class PartSelector extends SingleSelector

  sealed case class AllSelector() extends PartSelector
  sealed case class ElementSelector(name: String) extends PartSelector
  sealed case class IdSelector(id: String) extends SingleSelector
  sealed case class AttributeSelector(name: Option[String],
                                      attrs: Seq[(String, Option[String], Option[String])]) extends PartSelector

  sealed abstract class ComplexSelectorPart

  sealed case class ClassSelectorPart(part: PartSelector) extends ComplexSelectorPart
  sealed case class PseudoSelectorPart(pseudoClass: String, param: Seq[ComponentValue]) extends ComplexSelectorPart
  sealed case class ComplexSelector(firstPart: Option[PartSelector],
                                    parts: Seq[ComplexSelectorPart]) extends SingleSelector

  sealed case class MultipleSelector(firstSelector: SingleSelector,
                                     selectors: Seq[(String, SingleSelector)]) extends Selector

  sealed abstract class Rule

  sealed case class QualifiedRule(selector: Either[Selector, Seq[ComponentValue]],
                                  block: DeclarationList) extends Rule
  sealed case class AtRule(name: String, options: Seq[ComponentValue],
                           block: Option[Either[DeclarationList, RuleList]]) extends Rule
  sealed case class RuleList(rules: Seq[Rule])

  sealed case class Stylesheet(rules: Seq[Either[Rule, CToken]])

  sealed case class Declaration(name: String, value: Seq[ComponentValue], isImportant: Boolean)
  sealed case class DeclarationList(declarations: Seq[Either[Declaration, AtRule]])
}
