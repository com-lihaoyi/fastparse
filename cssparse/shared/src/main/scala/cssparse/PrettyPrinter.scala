package cssparse

import cssparse.Ast._

object PrettyPrinter {

  val indentSize = 2

  def printToken(token: SimpleToken): String = {
    token match {
      case IdentToken(name) => name
      case AtWordToken(name) => s"@$name"
      case HashWordToken(name) => s"#$name"
      case StringToken(string) => s"'$string'"
      case UrlToken(url) => s"url($url)"
      case NumberToken(number) => number
      case DimensionToken(number, dimensionName) => number + dimensionName
      case PercentageToken(number) => s"$number%"
      case UnicodeRangeToken(left, right) if left == right => s"U+$left"
      case UnicodeRangeToken(left, right) => s"U+$left-$right"
      case IncludeMatchToken() => "~="
      case DashMatchToken() => "|="
      case PrefixMatchToken() => "^="
      case SuffixMatchToken() => "$="
      case SubstringMatchToken() => "*="
      case ColumnToken() => "||"
      case CdcToken() => "<!--"
      case CdoToken() => "-->"
      case DelimToken(delim) => delim
    }
  }

  def printComponentValues(values: Seq[ComponentValue], indent: Int = 0, isIndentation: Boolean = true): String = {
    val indentPart = if (isIndentation) "\n" + " " * indentSize * indent else " "

    values.dropRight(1).map {
      case DelimToken(";") => ";" + indentPart
      case st: SimpleToken => printToken(st) + " "
      case block: CurlyBracketsBlock => printBlock(block, indent, isIndentation=isIndentation) + indentPart
      case block: Block => printBlock(block, isIndentation = false)
    }.mkString +
    (values.last match {
      case st: SimpleToken => printToken(st)
      case block: CurlyBracketsBlock => printBlock(block, indent, isIndentation=isIndentation, isLast = true)
      case block: Block => printBlock(block, isIndentation = false, isLast = true)
    })
  }

  def printBlock(block: Block, indent: Int = 0, isIndentation: Boolean = true, isLast: Boolean = false): String = {
    block.leftBracket +
      (if (isIndentation) "\n" + " " * indentSize * (indent + 1) else "") +
      printComponentValues(block.values, indent + 1, isIndentation=isIndentation) +
      (if (isIndentation) "\n" + " " * indentSize * indent else "") +
      block.rightBracket +
      (if (isIndentation && !isLast) "\n" else "")
  }

  def printRule(rule: Rule, indent: Int): String = {

    def indentBlock(block: Option[Block]): String = {
      block match {
        case Some(block: CurlyBracketsBlock) => printBlock(block, indent)
        case Some(block) => printBlock(block, isIndentation = false)
        case None => ""
      }
    }

    rule match {
      case atRule: AtRule => s"@${atRule.name} " +
        printComponentValues(atRule.selector, isIndentation = false) +
        indentBlock(atRule.block)
      case qualifiedRule: QualifiedRule => printComponentValues(qualifiedRule.selector, isIndentation = false) +
        indentBlock(Some(qualifiedRule.block))
      }
    }


  def printRuleList(ruleList: RuleList, indent: Int = 0): String = {
    ruleList.rules.map(printRule(_, indent)).mkString("\n")
  }
}
