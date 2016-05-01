package cssparse

import fastparse.all._

// According to https://www.w3.org/TR/css-syntax-3/

object CssTokensParser {

  val comment = P( "/*" ~/ (!"*/" ~ AnyChar).rep ~/ "*/")

  val newline = P( "\n" | "\r\n" | "\r" | "\f")

  val whitespace = P( " " | "\t" | newline)

  val hex_digit = P( CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))

  val escape = P( "\\" ~ ((!(newline | hex_digit) ~ AnyChar) |
    (hex_digit.rep(min=1, max=6) ~ whitespace.?)) )

  val whitespace_token = P( comment | whitespace.rep(1) )

  val ws = P( whitespace_token.rep )

  val ident_token = P( ("-".? ~ (CharIn('a' to 'z', 'A' to 'Z', "_") | escape) ~
    (CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "_-") | escape).rep).! ).map(Ast.IdentToken)

  val function_token = P( ident_token.! ~ "(" ).map(Ast.FunctionToken)

  val at_word_token = P( "@" ~ ident_token.! ).map(Ast.AtWordToken)

  val hash_token = P( "#" ~
    (CharIn('a' to 'z', 'A' to 'Z', '0' to '9', "_-") | escape).rep(1).! ).map(Ast.HashWordToken)

  val string_token_char = P( (!("\"" | "'" | "\\" | newline ) ~ AnyChar) | escape | ("\\" ~ newline) )

  val string_token = P( ("\"" ~ string_token_char.rep.! ~ "\"") |
    ("'" ~ string_token_char.rep.! ~ "'") ).map(Ast.StringToken)

  val url_unquoted = P( ((!(CharIn("\"\'()\\") | whitespace) ~ AnyChar) | escape).rep(1) )

  val url_token = P( "url(" ~ (ws ~ (url_unquoted.! | string_token.!) ~ ws).?.! ~ ")" ).map(Ast.UrlToken)

  val digit = P( CharIn('0' to '9') )

  val number_token = P( ((CharIn("+-").? ~ (digit.rep(1) ~ "." ~ digit.rep(1))) | digit.rep(1) | ("." ~ digit.rep(1) ~
    (CharIn("eE") ~ CharIn("+-").? ~ digit.rep(1)).?)).! ).map(Ast.NumberToken)

  val dimension_token = P( number_token.! ~ ident_token.! ) map
    { case (number, ident) => Ast.DimensionToken(number, ident)}

  val percentage_token = P( number_token.! ~ "%" ).map(Ast.PercentageToken)

  val unicode_range_token = P( CharIn("Uu") ~ "+" ~ hex_digit.rep(min=1, max=6).! |
    (hex_digit.rep(min=1, max=5).! flatMap (s => "?".rep(min=1, max=6 - s.length))).! |
    (hex_digit.rep(min=1, max=6).! ~ "-" ~ hex_digit.rep(min=1, max=6).!) ).map({ //TODO bad pattern matching
      case hex: String => Ast.UnicodeRangeToken(hex, hex)
      case (left: String, right: String) => Ast.UnicodeRangeToken(left, right)
    })

  val include_match_token =   P( "~=" ).map({_ => Ast.IncludeMatchToken()})
  val dash_match_token =      P( "|=" ).map({_ => Ast.DashMatchToken()})
  val prefix_match_token =    P( "^=" ).map({_ => Ast.PrefixMatchToken()})
  val suffix_match_token =    P( "$=" ).map({_ => Ast.SuffixMatchToken()})
  val substring_match_token = P( "*=" ).map({_ => Ast.SubstringMatchToken()})
  val match_token = P( include_match_token | dash_match_token |
                       prefix_match_token  | suffix_match_token |
                       suffix_match_token  | substring_match_token |
                       substring_match_token)

  val column_token =          P( "||" ).map({_ => Ast.ColumnToken()})
  val CDO_token =             P( "<!--" ).map({_ => Ast.CdoToken()})
  val CDC_token =             P( "-->" ).map({_ => Ast.CdcToken()})

  val delim_token = P( ("::" | CharIn("#$*+,-./:<>^~=!")).! ).map(Ast.DelimToken)

  // any token except function_token
  val simple_token: Parser[Option[Ast.SimpleToken]] = P(
    whitespace_token | percentage_token |
    dimension_token  | unicode_range_token |
    url_token        | at_word_token |
    hash_token       | string_token |
    ident_token      | number_token |
    match_token      | column_token |
    CDO_token        | CDC_token |
    delim_token ).map({
      case st: Ast.SimpleToken => Some(st)
      case _ => None
    })

  val bracketsBlock =       P( "(" ~ componentValue.rep ~ ")" ).map(values => Ast.BracketsBlock(values.flatten))
  val curlyBracketsBlock =  P( "{" ~ componentValue.rep ~ "}" ).map(values => Ast.CurlyBracketsBlock(values.flatten))
  val squareBracketsBlock = P( "[" ~ componentValue.rep ~ "]" ).map(values => Ast.SquareBracketsBlock(values.flatten))

  val functionBlock = P( function_token ~ componentValue.rep ~ ")").map({
    case (Ast.FunctionToken(name), values: Seq[Option[Ast.ComponentValue]]) =>
      Ast.FunctionBlock(name, Ast.BracketsBlock(values.flatten))
  })

  val componentValue: Parser[Option[Ast.ComponentValue]] = P( simple_token | bracketsBlock |
    curlyBracketsBlock | squareBracketsBlock | functionBlock ).map({
      case cv: Ast.ComponentValue => Some(cv)
      case Some(cv: Ast.ComponentValue) => Some(cv)
      case _ => None
    })
}

object CssRulesParser {

  import CssTokensParser._

  val allSelector = P( "*" ).map({_ => Ast.AllSelector()})

  val elementSelector = P( ident_token.! ).map(Ast.ElementSelector)

  val classSelector = P( ident_token.!.? ~ ("." ~ ident_token.!).rep(1) ).map({
    case (first_name, names) => Ast.ClassSelector(first_name, names)
  })

  val idSelector = P( "#" ~ ident_token.! ).map(Ast.IdSelector)

  val simpleSelector: Parser[Ast.SimpleSelector] = P( idSelector | classSelector | elementSelector )

  val attributeSelector = P( simpleSelector.? ~ ("[" ~ ident_token.! ~
    (( "=" | match_token).! ~ (string_token | ident_token)).? ~ "]").rep(1) ).map({
    case (sselector, attrs) => Ast.AttributeSelector(sselector, attrs.map({
      case (ident, Some((token, Ast.StringToken(string)))) => (ident, Some(token), Some(string))
      case (ident, Some((token, Ast.IdentToken(string)))) => (ident, Some(token), Some(string))
      case (ident, None) => (ident, None, None)
    }))
  })

  val pseudoSelector = P( (attributeSelector | simpleSelector).? ~ (("::" | ":").! ~ ws ~ ident_token.!)
                          ~ ("(" ~ componentValue ~ ")").? ).map({
    case (Some(s: Ast.SimpleSelector), (l, r), param) =>
      Ast.PseudoSelector(Some(Left(s)), l + r, param.flatMap(identity))
    case (Some(s: Ast.AttributeSelector), (l, r), param) =>
      Ast.PseudoSelector(Some(Right(s)), l + r, param.flatMap(identity))
    case (None, (l, r), param) =>
      Ast.PseudoSelector(None, l + r, param.flatMap(identity))
  })

  val singleSelector: Parser[Ast.SingleSelector] = P( pseudoSelector | attributeSelector | simpleSelector ).log()

  val selectorDelim = P( (ws ~ CharIn(",>+~").! ~ ws) | whitespace_token.rep(1).! ).map({
    case s if s.startsWith(" ") => " "
    case s => s
  }).log()

  val multipleSelector = P( singleSelector ~ (selectorDelim ~ singleSelector).rep(1) ).map({
    case (firstSelector, selectors) => Ast.MultipleSelector(firstSelector, selectors)
  }).log()

  val selector: Parser[Ast.Selector] = P(multipleSelector | singleSelector | allSelector).log()

  val important = P( "!" ~ ws ~ "important" ~ ws)

  val declaration = P( ident_token.! ~ ws ~ ":" ~ (!CharIn(";}!") ~ componentValue).rep ~ important.!.?).map({
    case (ident, values, Some(_)) => Ast.Declaration(ident, values.flatten, isImportant = true)
    case (ident, values, None) => Ast.Declaration(ident, values.flatten, isImportant = false)
  })

  val simpleAtRule = P( at_word_token ~ (!CharIn(";{}") ~ componentValue).rep ).map({
    case (Ast.AtWordToken(name), values) => Ast.AtRule(name, values.flatten, None)
  }).log()

  val declarationList = P( (ws ~ (simpleAtRule | declaration) ~ ws ~ (&("}") | ";")).rep ).map(
    s => Ast.DeclarationList(s.map({
      case atRule: Ast.AtRule => Right(atRule)
      case declaration: Ast.Declaration => Left(declaration)
    }))).log()

  val declAtRule =
    P( at_word_token ~ (!CharIn(";{}") ~ componentValue).rep ~ "{" ~ declarationList ~ ws ~ "}" ).map({
      case (Ast.AtWordToken(name), values, block) => Ast.AtRule(name, values.flatten, Some(Left(block)))
    }).log()

  val complexAtRule =
    P( at_word_token ~ (!CharIn(";{}") ~ componentValue).rep ~ "{" ~ ruleList ~ ws ~ "}" ).map({
      case (Ast.AtWordToken(name), values, block) => Ast.AtRule(name, values.flatten, Some(Right(block)))
    }).log()

  val atRule = P( complexAtRule | declAtRule | (simpleAtRule ~ ";") ).log()

  val qualifiedRule = P( ((selector ~ ws) | (!"{" ~ componentValue).rep) ~ "{" ~ declarationList ~ ws ~ "}" ).map({
    case (values: Seq[Option[Ast.ComponentValue]], block) => Ast.QualifiedRule(Right(values.flatten), block)
    case (selector: Ast.Selector, block) => Ast.QualifiedRule(Left(selector), block)
  }).log()

  val ruleList: Parser[Ast.RuleList] = P( (whitespace_token | atRule | qualifiedRule).rep ).map(
    s => Ast.RuleList(s flatMap {
      case rule: Ast.Rule => Some(rule)
      case _ => None
    }))

  val stylesheet = P( (CDO_token | CDC_token | whitespace_token | atRule | qualifiedRule).rep ).map(
    s => Ast.Stylesheet(s flatMap {
      case rule: Ast.Rule => Some(Left(rule))
      case ctoken: Ast.CToken => Some(Right(ctoken))
      case _ => None
    }))
}
