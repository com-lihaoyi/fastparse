package scalaParser
import acyclic.file
import parsing._
trait Exprs extends Core with Types with Xml{

  private implicit def wspStr(s: String) = WL ~ s


  def NewBody: R0
  def BlockDef: R0

  val Import: R0 = {
    val Selector: R0 = R( Id ~ (`=>` ~ (Id | `_`)).? )
    val Selectors: R0 = R( "{" ~ (Selector ~ ",").rep ~ (Selector | `_`) ~ "}" )
    val ImportExpr: R0 = R( StableId ~ ("." ~ (`_` | Selectors)).? )
    R( `import` ~! ImportExpr.rep1(",") )
  }

  val Ascription = R( `:` ~ (`_*` |  Type | Annot.rep1) )

  val LambdaHead: R0 = {
    val Binding = R( (Id | `_`) ~ (`:` ~ Type).? )
    val Bindings = R( "(" ~ Binding.rep(",") ~ ")" )
    val Implicit = R( `implicit`.? ~ Id ~ (`:` ~ InfixType).? )
    R( (Bindings | Implicit | `_` ~ Ascription.?) ~ `=>` )
  }
  object StatCtx extends WsCtx(true)
  object ExprCtx extends WsCtx(false)
  val TypeExpr = ExprCtx.Expr
  class WsCtx(injectSemicolons: Boolean){

    val OneSemiMax = if (injectSemicolons) OneNLMax else Parser.Pass
    val NoSemis = if (injectSemicolons) NotNewline else Parser.Pass

    val Enumerators = {
      val Generator = R( Pat1 ~ `<-` ~ Expr ~ Guard.? )
      val Assign = R( Pat1 ~ `=` ~ Expr )
      val Enumerator = R( Semis ~ Generator | Semis.? ~ Guard | Semis ~ Assign )
      R( Generator ~ Enumerator.rep ~ WL )
    }

    val Expr: R0 = {
      val If = {
        val Else = R( Semi.? ~ `else` ~ Expr )
        R( `if` ~ "(" ~ ExprCtx.Expr ~ ")" ~ Expr ~ Else.? )
      }
      val While = R( `while` ~ "(" ~ Expr ~ ")" ~ Expr )
      val Try = {
        val Catch = R( `catch` ~ Expr )
        val Finally = R( `finally` ~ Expr )
        R( `try` ~ Expr ~ Catch.? ~ Finally.? )
      }
      val DoWhile = R( `do` ~ Expr ~ Semi.? ~ `while` ~ "(" ~ Expr ~ ")" )

      val For = {
        val Body = R( "(" ~ ExprCtx.Enumerators ~ ")" | "{" ~ StatCtx.Enumerators ~ "}" )
        R( `for` ~ Body ~ `yield`.? ~ Expr )
      }
      val Throw = R( `throw` ~ Expr )
      val Return = R( `return` ~ Expr.? )

      val SmallerExpr = R( PostfixExpr ~ (`match` ~ "{" ~ CaseClauses ~ "}" | Ascription).? )
      val LambdaBody = R( If | While | Try | DoWhile | For | Throw | Return | SmallerExpr )
      R( LambdaHead.rep ~ LambdaBody )
    }

    val PostfixExpr: R0 = {
      val Prefixed = R( (WL ~ CharIn("-+~!") ~ WS ~ !syntax.Basic.OpChar) ~  SimpleExpr )
      val PrefixExpr = R( Prefixed | SimpleExpr )
      val InfixExpr = R( PrefixExpr ~ (NoSemis ~ Id ~ TypeArgs.? ~ OneSemiMax ~ PrefixExpr).rep)
      R( InfixExpr ~ (NotNewline ~ Id ~ Newline.?).? ~ (`=` ~ Expr).?)
    }

    val SimpleExpr: R0 = {
      val Path = R( (Id ~ ".").rep ~ `this` ~ ("." ~ Id).rep | StableId )
      val New = R( `new` ~ NewBody )
      val Parened = R ( "(" ~ Exprs.? ~ ")"  )
      val SimpleExpr1 = R( XmlExpr | New | BlockExpr | Literal | Path | `_` | Parened)
      R( SimpleExpr1 ~ ("." ~ Id | TypeArgs | NoSemis ~ ArgList).rep ~ (NoSemis  ~ `_`).?)
    }
    val Guard : R0 = R( `if` ~ PostfixExpr )
  }
  val SimplePat: R0 = {
    val ExtractorArgs = R( Pat.rep(",") )
    val Extractor = R( StableId ~ ("(" ~ ExtractorArgs ~ ")").? )
    val TupleEx = R( "(" ~ ExtractorArgs.? ~ ")" )
    val Thingy = R( `_` ~ (`:` ~ TypePat).? ~ !"*" )
    R( XmlPattern | Thingy | Literal | TupleEx | Extractor | VarId)
  }

  val BlockExpr: R0 = R( "{" ~ (CaseClauses | Block) ~ `}` )

  val BlockStats: R0 = {
    val Prelude = R( Annot.rep ~ `implicit`.? ~ `lazy`.? ~ LocalMod.rep )
    val Tmpl = R( Prelude ~ BlockDef )
    val BlockStat = R( Import | Tmpl | StatCtx.Expr )
    R( BlockStat.rep1(Semis) )
  }

  val Block: R0 = {
    val End = R( Semis.? ~ &("}" | `case`) )
    val ResultExpr = R{ StatCtx.Expr | LambdaHead ~ Block}
    val Body = R(
      ResultExpr ~ End |
      BlockStats ~ (Semis ~ ResultExpr).? ~ End |
      End
    )
    R( LambdaHead.rep ~ Semis.? ~ Body )
  }

  val Patterns: R0 = R( Pat.rep1(",") )
  val Pat: R0 = R( Pat1.rep1("|") )
  val Pat1: R0 = R( `_` ~ `:` ~ TypePat | VarId ~ `:` ~ TypePat | Pat2 )
  val Pat2: R0 = {
    val Pat3 = R( `_*` | SimplePat ~ (Id ~ SimplePat).rep )
    R( (VarId | `_`) ~ `@` ~ Pat3 | Pat3 | VarId )
  }

  val TypePat = R( CompoundType )

  val ArgList: R0 = R( "(" ~ (Exprs ~ (`:` ~ `_*`).?).? ~ ")" | OneNLMax ~ BlockExpr )

  val CaseClauses: R0 = {
    val CaseClause: R0 = R( `case` ~ Pat ~ ExprCtx.Guard.? ~ `=>` ~ Block )
    R( CaseClause.rep1 )
  }
}
