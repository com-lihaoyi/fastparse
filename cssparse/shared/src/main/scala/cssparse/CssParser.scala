package cssparse

import fastparse.all._

import scala.collection.mutable.ArrayBuffer

// According to https://www.w3.org/TR/css-syntax-3/

object CssTokensParser {

  val comment = P( "/*" ~/ (!"*/" ~ AnyChar).rep ~/ "*/")

  val newline = P( "\n" | "\r\n" | "\r" | "\f")

  val whitespace = P( " " | "\t" | newline)

  val hexDigit = P( CharIn('0' to '9', 'a' to 'f', 'A' to 'F') )

  val escape = P( "\\" ~ ((!(newline | hexDigit) ~ AnyChar) | (hexDigit.rep(min=1, max=6) ~ whitespace.?)) )

  val whitespaceToken = P( comment | whitespace.rep(1) )

  val ws = P( whitespaceToken.rep )

  val identToken = {
    val firstLetter = P( "-".? ~ (CharIn('a' to 'z', 'A' to 'Z', "_") | escape) )
    val rest = P( (CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "_-") | escape).rep )

    P( (firstLetter ~ rest).! ).map(Ast.IdentToken)
  }

  val functionToken = P( identToken.! ~ "(" ).map(Ast.FunctionToken)

  val atWordToken = P( "@" ~ identToken.! ).map(Ast.AtWordToken)

  val hashToken = P( "#" ~
    (CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "_-") | escape).rep(1).! ).map(Ast.HashWordToken)

  val stringTokenChar = P( (!("\"" | "'" | "\\" | newline ) ~ AnyChar) | escape | ("\\" ~ newline) )

  val stringToken = {
    val stringQuotes1 = P( "\"" ~ stringTokenChar.rep.! ~ "\"" )
    val stringQuotes2 = P( "'" ~ stringTokenChar.rep.! ~ "'" )

    P( stringQuotes1 | stringQuotes2 ).map(Ast.StringToken)
  }

  val urlUnquoted = P( ((!(CharIn("\"\'()\\") | whitespace) ~ AnyChar) | escape).rep(1) )

  val urlToken = P( "url(" ~ (ws ~ (urlUnquoted.! | stringToken.!) ~ ws).?.! ~ ")" ).map(Ast.UrlToken)

  val digit = P( CharIn('0' to '9') )

  val numberToken = {
    val withPoint = P( digit.rep(1) ~ "." ~ digit.rep(1) )
    val withoutPoint = P( digit.rep(1) )
    val withE = P( "." ~ digit.rep(1) ~ (CharIn("eE") ~ CharIn("+-").? ~ digit.rep(1)).? )
    P( (CharIn("+-").? ~ (withPoint | withoutPoint | withE)).! ).map(Ast.NumberToken)
  }

  val dimensionToken = P( numberToken.! ~ identToken.! ) map
    {case (number, ident) => Ast.DimensionToken(number, ident)}

  val percentageToken = P( numberToken.! ~ "%" ).map(Ast.PercentageToken)

  val unicodeRangeToken = {
    val questionMarks = P( (hexDigit.rep(min=1, max=6) ~ "?".rep(min=1, max=5)).! )
    val range = P( hexDigit.rep(min=1, max=6).! ~ "-" ~ hexDigit.rep(min=1, max=6).! )
    val regular = P( hexDigit.rep(min=1, max=6).! )

    P( CharIn("Uu") ~ "+" ~ (questionMarks | range | regular) ).map{
      case hex: String => Ast.UnicodeRangeToken(hex, hex)
      case (left: String, right: String) => Ast.UnicodeRangeToken(left, right)
    }
  }


  val includeMatchToken =   P( "~=" ).map{ _ => Ast.IncludeMatchToken()}
  val dashMatchToken =      P( "|=" ).map{ _ => Ast.DashMatchToken()}
  val prefixMatchToken =    P( "^=" ).map{ _ => Ast.PrefixMatchToken()}
  val suffixMatchToken =    P( "$=" ).map{_ => Ast.SuffixMatchToken()}
  val substringMatchToken = P( "*=" ).map{_ => Ast.SubstringMatchToken()}
  val matchToken = P(
    includeMatchToken | dashMatchToken |
    prefixMatchToken  | suffixMatchToken |
    suffixMatchToken  | substringMatchToken |
    substringMatchToken
  )

  val columnToken =          P( "||" ).map{_ => Ast.ColumnToken()}
  val CDOToken =             P( "<!--" ).map{_ => Ast.CdoToken()}
  val CDCToken =             P( "-->" ).map{_ => Ast.CdcToken()}

  val delimToken = P( ("::" | CharIn("#$*+,-./:<>^~=!")).! ).map(Ast.DelimToken)

  // any token except functionToken
  val simpleToken: Parser[Option[Ast.SimpleToken]] = P(
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

  val bracketsBlock =       P( "(" ~ componentValue.rep ~ ")" ).map(values => Ast.BracketsBlock(values.flatten))
  val curlyBracketsBlock =  P( "{" ~ componentValue.rep ~ "}" ).map(values => Ast.CurlyBracketsBlock(values.flatten))
  val squareBracketsBlock = P( "[" ~ componentValue.rep ~ "]" ).map(values => Ast.SquareBracketsBlock(values.flatten))

  val functionBlock = P( functionToken ~ componentValue.rep ~ ")").map{
    case (Ast.FunctionToken(name), values: Seq[Option[Ast.ComponentValue]]) =>
      Ast.FunctionBlock(name, Ast.BracketsBlock(values.flatten))
  }

  val componentValue: Parser[Option[Ast.ComponentValue]] = {
    val blockOpt = P( bracketsBlock | curlyBracketsBlock | squareBracketsBlock | functionBlock ).map(Some(_))
    P( simpleToken | blockOpt )
  }
}

object CssRulesParser {

  import CssTokensParser._

  val allSelector = P( "*" ).map{_ => Ast.AllSelector()}

  val elementSelector = P( identToken.! ).map(Ast.ElementSelector)

  val idSelector = P( "#" ~ identToken.! ).map(Ast.IdSelector)

  val attributeSelector = {
    val bracket = P( "[" ~ identToken.! ~ (( "=" | matchToken).! ~ (stringToken | identToken)).? ~ "]" )

    P( identToken.!.? ~ bracket.rep(1) ).map{
      case (name, attrs) => Ast.AttributeSelector(name, attrs.map{
        case (ident, Some((token, Ast.StringToken(string)))) => (ident, Some(token), Some(string))
        case (ident, Some((token, Ast.IdentToken(string)))) => (ident, Some(token), Some(string))
        case (ident, None) => (ident, None, None)
      })
    }
  }

  val partSelector = P( allSelector | attributeSelector | elementSelector )

  val classSelectorPart = P( "." ~  partSelector ).map(Ast.ClassSelectorPart)

  val pseudoSelectorPart = P( (("::" | ":") ~ identToken).! ~ ("(" ~ componentValue.rep(1) ~ ")").? ).map{
    case (name, optValues) =>
      Ast.PseudoSelectorPart(name, optValues.toSeq.flatten.flatten)
  }

  val complexSelectorPart = P( pseudoSelectorPart | classSelectorPart )

  val complexSelector = P( partSelector.? ~ complexSelectorPart.rep(1) ).map{
    case (part, parts) => Ast.ComplexSelector(part, parts)
  }

  val singleSelector: Parser[Ast.SingleSelector] = P( complexSelector | partSelector | idSelector | allSelector )

  val selectorDelim = P( (ws ~ CharIn(",>+~").! ~ ws) | whitespaceToken.rep(1).! ).map{
    case s if s.startsWith(" ") => " "
    case s => s
  }

  val multipleSelector = P( singleSelector ~ (selectorDelim ~ singleSelector).rep(1) ).map{
    case (firstSelector, selectors) => Ast.MultipleSelector(firstSelector, selectors)
  }

  val selector: Parser[Ast.Selector] = P( multipleSelector | singleSelector | allSelector )


  val important = P( "!" ~ ws ~ "important" ~ ws)

  val declaration = P( identToken.! ~ ws ~ ":" ~ (!CharIn(";}!") ~ componentValue).rep ~ important.!.?).map{
    case (ident, values, Some(_)) => Ast.Declaration(ident, values.flatten, isImportant = true)
    case (ident, values, None) => Ast.Declaration(ident, values.flatten, isImportant = false)
  }

  val simpleAtRule = P( atWordToken ~ (!CharIn(";{}") ~ componentValue).rep ).map{
    case (Ast.AtWordToken(name), values) => Ast.AtRule(name, values.flatten, None)
  }

  val declarationList = P( (ws ~ (simpleAtRule | declaration) ~ ws ~ (&("}") | ";")).rep ).map(
    s => Ast.DeclarationList(s.map{
      case atRule: Ast.AtRule => Right(atRule)
      case declaration: Ast.Declaration => Left(declaration)
    }))

  val declAtRule =
    P( atWordToken ~ (!CharIn(";{}") ~ componentValue).rep ~ "{" ~ declarationList ~ ws ~ "}" ).map{
      case (Ast.AtWordToken(name), values, block) => Ast.AtRule(name, values.flatten, Some(Left(block)))
    }

  val complexAtRule =
    P( atWordToken ~ (!CharIn(";{}") ~ componentValue).rep ~ "{" ~ ruleList ~ ws ~ "}" ).map{
      case (Ast.AtWordToken(name), values, block) => Ast.AtRule(name, values.flatten, Some(Right(block)))
    }

  val atRule = P( complexAtRule | declAtRule | (simpleAtRule ~ ";") )

  val qualifiedRule = P( ((selector ~ ws) | (!"{" ~ componentValue).rep) ~ "{" ~ declarationList ~ ws ~ "}" ).map{
    case (values: Seq[Option[Ast.ComponentValue]], block) => Ast.QualifiedRule(Right(values.flatten), block)
    case (selector: Ast.Selector, block) => Ast.QualifiedRule(Left(selector), block)
  }

  val ruleList: Parser[Ast.RuleList] = P( (whitespaceToken | atRule | qualifiedRule).rep ).map{
    s => Ast.RuleList(s flatMap {
      case rule: Ast.Rule => Some(rule)
      case _ => None
    })
  }

  val stylesheet = P( (CDOToken | CDCToken | whitespaceToken | atRule | qualifiedRule).rep ).map{
    s => Ast.Stylesheet(s flatMap {
      case rule: Ast.Rule => Some(Left(rule))
      case ctoken: Ast.CToken => Some(Right(ctoken))
      case _ => None
    })
  }
}
