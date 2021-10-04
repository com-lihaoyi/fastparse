package cssparse

import fastparse._

// According to https://www.w3.org/TR/css-syntax-3/

import fastparse.NoWhitespace._

object CssTokensParser {

  def comment[_: P] = P( "/*" ~ (!"*/" ~ AnyChar).rep ~ "*/")

  def newline[_: P] = P( "\n" | "\r\n" | "\r" | "\f")

  def whitespace[_: P] = P( " " | "\t" | newline)

  def hexDigit[_: P] = P( CharIn("0-9", "a-f", "A-F") )

  def escape[_: P] = P( "\\" ~ ((!(newline | hexDigit) ~ AnyChar) | (hexDigit.rep(min=1, max=6) ~ whitespace.?)) )

  def whitespaceToken[_: P] = P( comment | whitespace.rep(1) )

  def ws[_: P] = P( whitespaceToken.rep )

  def identToken[_: P] = {
    def firstLetter = P( "-".? ~ (CharIn("a-zA-Z_") | escape) )
    def rest = P( (CharIn("a-zA-Z0-9\\-_") | escape).rep )

    P( (firstLetter ~ rest).! ).map(Ast.IdentToken)
  }

  def functionToken[_: P] = P( identToken.! ~ "(" ).map(Ast.FunctionToken)

  def atWordToken[_: P] = P( "@" ~ identToken.! ).map(Ast.AtWordToken)

  def hashToken[_: P] = P( "#" ~
    (CharIn("a-zA-Z0-9\\-_") | escape).rep(1).! ).map(Ast.HashWordToken)

  def stringTokenChar[_: P] = P( (!("\"" | "'" | "\\" | newline ) ~ AnyChar) | escape | ("\\" ~ newline) )

  def stringToken[_: P] = {
    def stringQuotes1 = P( "\"" ~ stringTokenChar.rep.! ~ "\"" )
    def stringQuotes2 = P( "'" ~ stringTokenChar.rep.! ~ "'" )

    P( stringQuotes1 | stringQuotes2 ).map(Ast.StringToken)
  }

  def urlUnquoted[_: P] = P( ((!(CharIn("\"\'()\\\\") | whitespace) ~ AnyChar) | escape).rep(1) )

  def urlToken[_: P] = P( "url(" ~ (ws ~ (urlUnquoted.! | stringToken.!) ~ ws).?.! ~ ")" ).map(Ast.UrlToken)

  def digit[_: P] = P( CharIn("0-9") )

  def numberToken[_: P] = {
    def withPoint = P( digit.rep(1) ~ "." ~ digit.rep(1) )
    def withoutPoint = P( digit.rep(1) )
    def withE = P( "." ~ digit.rep(1) ~ (CharIn("eE") ~ CharIn("+\\-").? ~ digit.rep(1)).? )
    P( (CharIn("+\\-").? ~ (withPoint | withoutPoint | withE)).! ).map(Ast.NumberToken)
  }

  def dimensionToken[_: P] = P( numberToken.! ~ identToken.! ) map
    {case (number, ident) => Ast.DimensionToken(number, ident)}

  def percentageToken[_: P] = P( numberToken.! ~ "%" ).map(Ast.PercentageToken)

  def unicodeRangeToken[_: P] = {
    def questionMarks = P( (hexDigit.rep(min=1, max=6) ~ "?".rep(min=1, max=5)).! )
    def range = P( hexDigit.rep(min=1, max=6).! ~ "-" ~ hexDigit.rep(min=1, max=6).! )
    def regular = P( hexDigit.rep(min=1, max=6).! )

    P( CharIn("Uu") ~ "+" ~ (questionMarks | range | regular) ).map{
      case hex: String => Ast.UnicodeRangeToken(hex, hex)
      case (left: String, right: String) => Ast.UnicodeRangeToken(left, right)
    }
  }


  def includeMatchToken[_: P] =   P( "~=" ).map{ _ => Ast.IncludeMatchToken()}
  def dashMatchToken[_: P] =      P( "|=" ).map{ _ => Ast.DashMatchToken()}
  def prefixMatchToken[_: P] =    P( "^=" ).map{ _ => Ast.PrefixMatchToken()}
  def suffixMatchToken[_: P] =    P( "$=" ).map{_ => Ast.SuffixMatchToken()}
  def substringMatchToken[_: P] = P( "*=" ).map{_ => Ast.SubstringMatchToken()}
  def matchToken[_: P] = P(
    includeMatchToken | dashMatchToken |
    prefixMatchToken  | suffixMatchToken |
    suffixMatchToken  | substringMatchToken |
    substringMatchToken
  )

  def columnToken[_: P] =          P( "||" ).map{_ => Ast.ColumnToken()}
  def CDOToken[_: P] =             P( "<!--" ).map{_ => Ast.CdoToken()}
  def CDCToken[_: P] =             P( "-->" ).map{_ => Ast.CdcToken()}

  def delimToken[_: P] = P( ("::" | CharIn("#$*+,\\-./:<>^~=!")).! ).map(Ast.DelimToken)

  // any token except functionToken
  def simpleToken[_: P]: P[Option[Ast.SimpleToken]] = P(
    whitespaceToken     | atWordToken |
    hashToken           | matchToken |
    columnToken         | CDOToken |
    CDCToken            | stringToken |
    unicodeRangeToken   | percentageToken |
    dimensionToken      | urlToken |
    numberToken         | identToken |
    delimToken
  ).map{
    case st: Ast.SimpleToken => Some(st)
    case _ => None
  }

  def bracketsBlock[_: P] =       P( "(" ~ componentValue.rep ~ ")" ).map(values => Ast.BracketsBlock(values.flatten))
  def curlyBracketsBlock[_: P] =  P( "{" ~ componentValue.rep ~ "}" ).map(values => Ast.CurlyBracketsBlock(values.flatten))
  def squareBracketsBlock[_: P] = P( "[" ~ componentValue.rep ~ "]" ).map(values => Ast.SquareBracketsBlock(values.flatten))

  def functionBlock[_: P] = P( functionToken ~ componentValue.rep ~ ")").map{
    case (Ast.FunctionToken(name), values: Seq[Option[Ast.ComponentValue]]) =>
      Ast.FunctionBlock(name, Ast.BracketsBlock(values.flatten))
  }

  def componentValue[_: P]: P[Option[Ast.ComponentValue]] = {
    def blockOpt = P( bracketsBlock | curlyBracketsBlock | squareBracketsBlock | functionBlock ).map(Some(_))
    P( simpleToken | blockOpt )
  }
}

object CssRulesParser {

  import CssTokensParser._

  def allSelector[_: P] = P( "*" ).map{_ => Ast.AllSelector()}

  def elementSelector[_: P] = P( identToken.! ).map(Ast.ElementSelector)

  def idSelector[_: P] = P( "#" ~ identToken.! ).map(Ast.IdSelector)

  def attributeSelector[_: P] = {
    def bracket = P( "[" ~ identToken.! ~ (( "=" | matchToken).! ~ (stringToken | identToken)).? ~ "]" )

    P( identToken.!.? ~ bracket.rep(1) ).map{
      case (name, attrs) => Ast.AttributeSelector(name, attrs.map{
        case (ident, Some((token, Ast.StringToken(string)))) => (ident, Some(token), Some(string))
        case (ident, Some((token, Ast.IdentToken(string)))) => (ident, Some(token), Some(string))
        case (ident, None) => (ident, None, None)
      })
    }
  }

  def partSelector[_: P] = P( allSelector | attributeSelector | elementSelector )

  def classSelectorPart[_: P] = P( "." ~  partSelector ).map(Ast.ClassSelectorPart)

  def pseudoSelectorPart[_: P] = P( (("::" | ":") ~ identToken).! ~ ("(" ~ componentValue.rep(1) ~ ")").? ).map{
    case (name, optValues) =>
      Ast.PseudoSelectorPart(name, optValues.toSeq.flatten.flatten)
  }

  def complexSelectorPart[_: P] = P( pseudoSelectorPart | classSelectorPart )

  def complexSelector[_: P] = P( partSelector.? ~ complexSelectorPart.rep(1) ).map{
    case (part, parts) => Ast.ComplexSelector(part, parts)
  }

  def singleSelector[_: P]: P[Ast.SingleSelector] = P( complexSelector | partSelector | idSelector | allSelector )

  def selectorDelim[_: P] = P( (ws ~ CharIn(",>+~").! ~ ws) | whitespaceToken.rep(1).! ).map{
    case s if s.startsWith(" ") => " "
    case s => s
  }

  def multipleSelector[_: P] = P( singleSelector ~ (selectorDelim ~ singleSelector).rep(1) ).map{
    case (firstSelector, selectors) => Ast.MultipleSelector(firstSelector, selectors)
  }

  def selector[_: P]: P[Ast.Selector] = P( multipleSelector | singleSelector | allSelector )


  def important[_: P] = P( "!" ~ ws ~ "important" ~ ws)

  def declaration[_: P] = P( identToken.! ~ ws ~ ":" ~ (!CharIn(";}!") ~ componentValue).rep ~ important.!.?).map{
    case (ident, values, Some(_)) => Ast.Declaration(ident, values.flatten, isImportant = true)
    case (ident, values, None) => Ast.Declaration(ident, values.flatten, isImportant = false)
  }

  def simpleAtRule[_: P] = P( atWordToken ~ (!CharIn(";{}") ~ componentValue).rep ).map{
    case (Ast.AtWordToken(name), values) => Ast.AtRule(name, values.flatten, None)
  }

  def declarationList[_: P] = P( (ws ~ (simpleAtRule | declaration) ~ ws ~ (&("}") | ";")).rep ).map(
    s => Ast.DeclarationList(s.map{
      case atRule: Ast.AtRule => Right(atRule)
      case declaration: Ast.Declaration => Left(declaration)
    }))

  def declAtRule[_: P] =
    P( atWordToken ~ (!CharIn(";{}") ~ componentValue).rep ~ "{" ~ declarationList ~ ws ~ "}" ).map{
      case (Ast.AtWordToken(name), values, block) => Ast.AtRule(name, values.flatten, Some(Left(block)))
    }

  def complexAtRule[_: P] =
    P( atWordToken ~ (!CharIn(";{}") ~ componentValue).rep ~ "{" ~ ruleList ~ ws ~ "}" ).map{
      case (Ast.AtWordToken(name), values, block) => Ast.AtRule(name, values.flatten, Some(Right(block)))
    }

  def atRule[_: P] = P( complexAtRule | declAtRule | (simpleAtRule ~ ";") )

  def qualifiedRule[_: P] = P( ((selector ~ ws) | (!"{" ~ componentValue).rep) ~ "{" ~ declarationList ~ ws ~ "}" ).map{
    case (values: Seq[Option[Ast.ComponentValue]], block) => Ast.QualifiedRule(Right(values.flatten), block)
    case (selector: Ast.Selector, block) => Ast.QualifiedRule(Left(selector), block)
  }

  def ruleList[_: P]: P[Ast.RuleList] = P( (whitespaceToken | atRule | qualifiedRule).rep ).map{
    s => Ast.RuleList(s flatMap {
      case rule: Ast.Rule => Some(rule)
      case _ => None
    })
  }

  def stylesheet[_: P] = P( (CDOToken | CDCToken | whitespaceToken | atRule | qualifiedRule).rep ).map{
    s => Ast.Stylesheet(s flatMap {
      case rule: Ast.Rule => Some(Left(rule))
      case ctoken: Ast.CToken => Some(Right(ctoken))
      case _ => None
    })
  }
}
