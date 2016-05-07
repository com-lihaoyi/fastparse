package cssparse

import cssparse.Ast._

object PrettyPrinter {

  val indentSize = 2
  def indentation(indent: Int, isIndentation: Boolean): String =
    if (isIndentation) "\n" + " " * indentSize * indent else " "

  def printToken(token: SimpleToken): String = {
    token match {
      case IdentToken("and") => "and "
      case IdentToken("or") => " or "
      case DelimToken(",") => ", "
      case DelimToken(".") => "."
      case DelimToken(":") => ":"
      case DelimToken("::") => "::"
      case DelimToken("=") => "="
      case DelimToken(";") => ";"
      case DelimToken(delim) => s" $delim "

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
    }
  }

  def printComponentValues(values: Seq[ComponentValue]): String = {
    if (values.nonEmpty) {
      def printBlock(block: Block): String =
        block.leftBracket + printComponentValues(block.values) + block.rightBracket

      values.dropRight(1).zip(values.drop(1)).map(p => {
        val (first, last) = p
        first match {
          case DelimToken(";") => "; "
          case st: SimpleToken =>
            val isTokenFirst = first.isInstanceOf[Ast.SimpleToken] && !first.isInstanceOf[DelimToken]
            val isTokenLast = last.isInstanceOf[Ast.SimpleToken] && !last.isInstanceOf[DelimToken]
            printToken(st) + (if (isTokenFirst && isTokenLast) " " else "")
          case block: Block => printBlock(block)
          case FunctionBlock(name, block) => name + " " + printBlock(block)
        }
      }).mkString +
        (values.last match {
          case st: SimpleToken => printToken(st)
          case block: Block => printBlock(block)
          case FunctionBlock(name, block) => name + " " + printBlock(block)
        })
    } else {
      ""
    }
  }

  def printSelector(selector: Selector): String = {

    def printPart(part: ComplexSelectorPart): String = {
      part match {
        case ClassSelectorPart(part) => "." + printSelector(part)
        case PseudoSelectorPart(pseudoClass, param) => pseudoClass +
          (if (param.nonEmpty) "(" + printComponentValues(param) + ")" else "")
      }
    }

    selector match {
      case AllSelector() => "*"
      case ElementSelector(name) => name
      case IdSelector(id) => "#" + id
      case AttributeSelector(optName, attrs) =>
        optName.getOrElse("") + attrs.map({
          case (attr, optToken, optValue) =>
            "[" + attr + optToken.getOrElse("") + optValue.map("\"" + _ + "\"").getOrElse("") + "]"
        }).mkString
      case ComplexSelector(firstPart, parts) =>
        firstPart.map(printSelector).getOrElse("") + parts.map(printPart).mkString
      case MultipleSelector(firstSelector, selectors) =>
        printSelector(firstSelector) +
        selectors.map({
          case (sep, selector) =>
            (sep match {
              case " " => " "
              case "," => ", "
              case s => " " + s + " "
            }) + printSelector(selector)
        }).mkString
    }
  }

  def printDeclarationList(list: DeclarationList, indent: Int = 0, isIndentation: Boolean = true): String = {
    val indentPart = indentation(indent, isIndentation)
    list.declarations.map({
      case Left(Declaration(name, values, isImportant)) =>
        indentPart + name + ": " + printComponentValues(values) + {if (isImportant) " ! important" else ""} + ";"
      case Right(atRule) => printRule(atRule)
    }).mkString
  }

  def printRule(rule: Rule, indent: Int = 0, isIndentation: Boolean = true): String = {
    val indentPart = indentation(indent, isIndentation)
    def indentBlock[T](block: T, printBlock: (T, Int, Boolean) => String) =
      " {" + printBlock(block, indent + 1, isIndentation) + indentPart + "}"

    rule match {
      case QualifiedRule(Left(selector), block) =>
        printSelector(selector) + indentBlock(block, printDeclarationList)
      case QualifiedRule(Right(values), block) =>
        printComponentValues(values) + indentBlock(block, printDeclarationList)
      case AtRule(name, options, None) =>
        indentPart + "@" + name + " " + printComponentValues(options) + ";"
      case AtRule(name, options, Some(Left(declartions))) =>
        indentPart + "@" + name + " " + printComponentValues(options) + indentBlock(declartions, printDeclarationList)
      case AtRule(name, options, Some(Right(rules))) =>
        indentPart + "@" + name + " " + printComponentValues(options) + indentBlock(rules, printRuleList)
      }
    }

  def printRuleList(ruleList: RuleList, indent: Int = 0, isIndentation: Boolean = true): String = {
    val indentPart = indentation(indent, isIndentation)
    ruleList.rules.map(rule => indentPart + printRule(rule, indent, isIndentation = isIndentation)).mkString("\n")
  }
}
