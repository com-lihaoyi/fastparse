package scalaParser
import acyclic.file
import parsing.Parsing.Parser.Pass
import parsing.Parsing._
trait Exprs extends Core with Types with Xml{

  private implicit def wspStr(s: String) = WL ~ s
  private implicit def wspCh(s: Char) = WL ~ s

  def NewBody: R0
  def BlockDef: R0

  val Import: R0 = {
    val Selector: R0 = rule( Id ~ (`=>` ~ (Id | `_`)).? )
    val Selectors: R0 = rule( '{' ~ (Selector ~ ',').rep ~ (Selector | `_`) ~ "}" )
    val ImportExpr: R0 = rule( StableId ~ ('.' ~ (`_` | Selectors)).? )
    rule( `import` ~ ImportExpr.rep1(',') )
  }

  val Ascription = rule( `:` ~ (`_*` |  Type | Annot.rep1) )

  val LambdaHead: R0 = {
    val Binding = rule( (Id | `_`) ~ (`:` ~ Type).? )
    val Bindings = rule( '(' ~ Binding.rep(',') ~ ')' )
    val Implicit = rule( `implicit`.? ~ Id ~ (`:` ~ InfixType).? )
    rule( (Bindings | Implicit | `_` ~ Ascription.?) ~ `=>` )
  }
  object StatCtx extends WsCtx(true)
  object ExprCtx extends WsCtx(false)
  val TypeExpr = ExprCtx.Expr
  class WsCtx(injectSemicolons: Boolean){

    val OneSemiMax = if (injectSemicolons) OneNLMax else Parser.Pass
    val NoSemis = if (injectSemicolons) NotNewline else Parser.Pass

    val Enumerators = {
      val Generator = rule( Pat1 ~ `<-` ~ Expr ~ Guard.? )
      val Assign = rule( Pat1 ~ `=` ~ Expr )
      val Enumerator = rule( Semis ~ Generator | Semis.? ~ Guard | Semis ~ Assign )
      rule( Generator ~ Enumerator.rep ~ WL )
    }

    val Expr: R0 = {
      val If = {
        val Else = rule( Semi.? ~ `else` ~ Expr )
        rule( `if` ~ '(' ~ ExprCtx.Expr ~ ')' ~ Expr ~ Else.? )
      }
      val While = rule( `while` ~ '(' ~ Expr ~ ')' ~ Expr )
      val Try = {
        val Catch = rule( `catch` ~ Expr )
        val Finally = rule( `finally` ~ Expr )
        rule( `try` ~ Expr ~ Catch.? ~ Finally.? )
      }
      val DoWhile = rule( `do` ~ Expr ~ Semi.? ~ `while` ~ '(' ~ Expr ~ ")" )

      val For = {
        val Body = rule( '(' ~ ExprCtx.Enumerators ~ ')' | '{' ~ StatCtx.Enumerators ~ '}' )
        rule( `for` ~ Body ~ `yield`.? ~ Expr )
      }
      val Throw = rule( `throw` ~ Expr )
      val Return = rule( `return` ~ Expr.? )

      val SmallerExpr = rule( PostfixExpr ~ (`match` ~ '{' ~ CaseClauses ~ "}" | Ascription).? )
      val LambdaBody = rule( If | While | Try | DoWhile | For | Throw | Return | SmallerExpr )
      rule( LambdaHead.rep ~ LambdaBody )
    }

    val PostfixExpr: R0 = {
      val Prefixed = rule( (WL ~ ("-"|"+"|"~"|"!") ~ WS ~ !syntax.Basic.OpChar) ~  SimpleExpr )
      val PrefixExpr = rule( Prefixed | SimpleExpr )
      val InfixExpr = rule( PrefixExpr ~ (NoSemis ~ Id ~ TypeArgs.? ~ OneSemiMax ~ PrefixExpr).rep)
      rule( InfixExpr ~ (NotNewline ~ Id ~ Newline.?).? ~ (`=` ~ Expr).?)
    }

    val SimpleExpr: R0 = {
      val Path = rule( (Id ~ '.').rep ~ `this` ~ ('.' ~ Id).rep | StableId )
      val New = rule( `new` ~ NewBody )
      val Parened = rule ( '(' ~ Exprs.? ~ ")"  )
      val SimpleExpr1 = rule( XmlExpr | New | BlockExpr | Literal | Path | `_` | Parened)
      rule( SimpleExpr1 ~ ('.' ~ Id | TypeArgs | NoSemis ~ ArgList).rep ~ (NoSemis  ~ `_`).?)
    }
    val Guard : R0 = rule( `if` ~ PostfixExpr )
  }
  val SimplePat: R0 = {
    val ExtractorArgs = rule( Pat.rep(',') )
    val Extractor = rule( StableId ~ ('(' ~ ExtractorArgs ~ ')').? )
    val TupleEx = rule( '(' ~ ExtractorArgs.? ~ ')' )
    val Thingy = rule( `_` ~ (`:` ~ TypePat).? ~ !"*" )
    rule( XmlPattern | Thingy | Literal | TupleEx | Extractor | VarId)
  }

  val BlockExpr: R0 = rule( '{' ~ (CaseClauses | Block) ~ `}` )

  val BlockStats: R0 = {
    val Prelude = rule( Annot.rep ~ `implicit`.? ~ `lazy`.? ~ LocalMod.rep )
    val Tmpl = rule( Prelude ~ BlockDef )
    val BlockStat = rule( Import | Tmpl | StatCtx.Expr )
    rule( BlockStat.rep1(Semis) )
  }

  val Block: R0 = {
    val End = rule( Semis.? ~ &("}" | `case`) )
    val ResultExpr = rule{ StatCtx.Expr | LambdaHead ~ Block}
    val Body = rule(
      ResultExpr ~ End |
      BlockStats ~ (Semis ~ ResultExpr).? ~ End |
      End
    )
    rule( LambdaHead.rep ~ Semis.? ~ Body )
  }

  val Patterns: R0 = rule( Pat.rep1(",") )
  val Pat: R0 = rule( Pat1.rep1('|') )
  val Pat1: R0 = rule( `_` ~ `:` ~ TypePat | VarId ~ `:` ~ TypePat | Pat2 )
  val Pat2: R0 = {
    val Pat3 = rule( `_*` | SimplePat ~ (Id ~ SimplePat).rep )
    rule( (VarId | `_`) ~ `@` ~ Pat3 | Pat3 | VarId )
  }

  val TypePat = rule( CompoundType )

  val ArgList: R0 = rule( '(' ~ (Exprs ~ (`:` ~ `_*`).?).? ~ ")" | OneNLMax ~ BlockExpr )

  val CaseClauses: R0 = {
    val CaseClause: R0 = rule( `case` ~ Pat ~ ExprCtx.Guard.? ~ `=>` ~ Block )
    rule( CaseClause.rep1 )
  }
}
