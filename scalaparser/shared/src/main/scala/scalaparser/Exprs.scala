package scalaparser

import acyclic.file
import fastparse._
trait Exprs extends Core with Types with Xml{

  private implicit def wspStr(s: String) = R(WL ~ s)(Utils.literalize(s).toString)

  def ClsTmpl: R0
  def BlockDef: R0

  val Import: R0 = {
    val Selector: R0 = R( Id ~ (`=>` ~ (Id | `_`)).? )
    val Selectors: R0 = R( "{" ~! (Selector ~ ",").rep ~ (Selector | `_`) ~ "}" )
    val ImportExpr: R0 = R( StableId ~ ("." ~! (`_` | Selectors)).? )
    R( `import` ~! ImportExpr.rep1(",") )
  }

  object StatCtx extends WsCtx(curlyBlock=true)
  object ExprCtx extends WsCtx(curlyBlock=false)

  val TypeExpr = ExprCtx.Expr

  class WsCtx(curlyBlock: Boolean){

    val OneSemiMax = if (curlyBlock) OneNLMax else Parser.Pass
    val NoSemis = if (curlyBlock) NotNewline else Parser.Pass

    val Enumerators = {
      val Generator = R( TypeOrBindPattern ~ `<-` ~! Expr ~ Guard.? )
      val Assign = R( TypeOrBindPattern ~ `=` ~! Expr )
      val Enumerator = R( Semis ~ Generator | Semis.? ~ Guard | Semis ~ Assign )
      R( Generator ~ Enumerator.rep ~ WL )
    }

    val Expr: R0 = {
      val If = {
        val Else = R( Semi.? ~ `else` ~ Expr )
        R( `if` ~! "(" ~ ExprCtx.Expr ~ ")" ~ Expr ~ Else.? )
      }
      val While = R( `while` ~! "(" ~ Expr ~ ")" ~ Expr )
      val Try = {
        val Catch = R( `catch` ~! Expr )
        val Finally = R( `finally` ~! Expr )
        R( `try` ~! Expr ~ Catch.? ~ Finally.? )
      }
      val DoWhile = R( `do` ~! Expr ~ Semi.? ~ `while` ~ "(" ~ Expr ~ ")" )

      val For = {
        val Body = R( "(" ~! ExprCtx.Enumerators ~ ")" | "{" ~! StatCtx.Enumerators ~ "}" )
        R( `for` ~! Body ~ `yield`.? ~ Expr )
      }
      val Throw = R( `throw` ~! Expr )
      val Return = R( `return` ~! Expr.? )
      val LambdaRhs = if (curlyBlock) R( BlockStat ) else R( Expr )


      val ImplicitLambda = R( `implicit` ~ (Id | `_`) ~ (`:` ~ InfixType).? ~ `=>` ~ LambdaRhs.? )
      val ParenedLambda = R( Parened ~ (`=>` ~ LambdaRhs.? | ExprSuffix ~ PostfixSuffix) )
      val PostfixLambda = R( PostfixExpr ~ (`=>` ~ LambdaRhs.?).? )
      val SmallerExprOrLambda = R( ParenedLambda | PostfixLambda )
      R(
        If | While | Try | DoWhile | For | Throw | Return |
        ImplicitLambda | SmallerExprOrLambda
      )
    }
    val AscriptionType = if (curlyBlock) R( InfixType ) else R( Type )
    val Ascription = R( `:` ~! (`_*` |  AscriptionType | Annot.rep1) )
    val MatchAscriptionSuffix = R(`match` ~! "{" ~ CaseClauses ~ "}" | Ascription)
    val ExprPrefix = R( WL ~ CharIn("-+~!") ~ WS ~ !syntax.Basic.OpChar )
    val ExprSuffix = R( ("." ~! Id | TypeArgs | NoSemis ~ ArgList).rep ~ (NoSemis  ~ `_`).? )
    val PrefixExpr = R( ExprPrefix.? ~ SimpleExpr )
    val InfixSuffix = R( NoSemis ~ Id ~ TypeArgs.? ~ OneSemiMax ~ PrefixExpr ~ ExprSuffix)
    val PostFix = R( NotNewline ~ Id ~ Newline.? )
    val PostfixSuffix = R( InfixSuffix.rep ~ PostFix.? ~ (`=` ~! Expr).? ~ MatchAscriptionSuffix.?)

    val PostfixExpr: R0 = R( PrefixExpr ~ ExprSuffix ~ PostfixSuffix)

    val Parened = R ( "(" ~! Exprs.? ~ ")" )
    val SimpleExpr: R0 = {
      val Path = R( (Id ~ ".").rep ~ `this` ~ ("." ~! Id).rep | StableId )
      val New = R( `new` ~! ClsTmpl )

      R( XmlExpr | New | BlockExpr | ExprLiteral | Path | `_` | Parened)
    }
    val Guard : R0 = R( `if` ~! PostfixExpr )
  }
  val SimplePattern: R0 = {
    val ExtractorArgs = R( Pattern.rep(",") )
    val TupleEx = R( "(" ~ ExtractorArgs ~ ")" )
    val Extractor = R( StableId ~ TypeArgs.? ~ TupleEx.? )
    val Thingy = R( `_` ~ (`:` ~ TypePat).? ~ !"*" )
    R( XmlPattern | Thingy | PatLiteral | TupleEx | Extractor | VarId)
  }

  val BlockExpr: R0 = R( "{" ~! (CaseClauses | Block) ~ `}` )

  val BlockStat = {
    val Prelude = R( Annot.rep ~ `implicit`.? ~ `lazy`.? ~ LocalMod.rep )
    val Tmpl = R( Prelude ~ BlockDef )
    R( Import | Tmpl | StatCtx.Expr )
  }

  val Block: R0 = {
    val BlockEnd = R( Semis.? ~ &("}" | `case`) )
    val Body = R( BlockStat.rep(Semis) )
    R( Semis.? ~ Body ~! BlockEnd )
  }

  val Patterns: R0 = R( Pattern.rep1(",") )
  val Pattern: R0 = R( TypeOrBindPattern.rep1("|" ~! Pass) )
  val TypePattern = R( (`_` | VarId) ~ `:` ~ TypePat )
  val TypeOrBindPattern: R0 = R( TypePattern | BindPattern )
  val BindPattern: R0 = {
    val InfixPattern = R( `_*` | SimplePattern ~ (Id ~ SimplePattern).rep )
    val Binding = R( (VarId | `_`) ~ `@` )
    R( Binding ~ InfixPattern | InfixPattern | VarId )
  }

  val TypePat = R( CompoundType )
  val ParenArgList = R( "(" ~! (Exprs ~ (`:` ~! `_*`).?).? ~ ")" )
  val ArgList: R0 = R( ParenArgList | OneNLMax ~ BlockExpr )

  val CaseClauses: R0 = {
    // Need to lookahead for `class` and `object` because
    // the block { case object X } is not a case clause!
    val CaseClause: R0 = R( `case` ~ !(`class` | `object`) ~! Pattern ~ ExprCtx.Guard.? ~ `=>` ~ Block )
    R( CaseClause.rep1 )
  }
}
