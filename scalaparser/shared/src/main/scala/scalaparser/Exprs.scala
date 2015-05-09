package scalaparser

import acyclic.file
import fastparse._
trait Exprs extends Core with Types with Xml{

  private implicit def wspStr(s: String) = P(WL ~ s)(Utils.literalize(s).toString)

  def AnonTmpl: P0
  def BlockDef: P0

  val Import: P0 = {
    val Selector: P0 = P( Id ~ (`=>` ~! (Id | `_`)).? )
    val Selectors: P0 = P( "{" ~! (Selector | `_` ).rep("," ~! Pass) ~ "}" )
    val ImportExpr: P0 = P( StableId ~ ("." ~! (`_` | Selectors)).? )
    P( `import` ~! ImportExpr.rep1("," ~! Pass) )
  }

  object StatCtx extends WsCtx(curlyBlock=true)
  object ExprCtx extends WsCtx(curlyBlock=false)

  val TypeExpr = ExprCtx.Expr

  class WsCtx(curlyBlock: Boolean){

    val OneSemiMax = if (curlyBlock) OneNLMax else Parser.Pass
    val NoSemis = if (curlyBlock) NotNewline else Parser.Pass

    val Enumerators = {
      val Generator = P( `<-` ~! Expr ~ Guard.? )
      val Assign = P( `=` ~! Expr )
      val Enumerator = P( Semis ~ TypeOrBindPattern ~! (Generator | Assign) | Semis.? ~ Guard  )
      P( TypeOrBindPattern ~ Generator ~ Enumerator.rep ~ WL )
    }

    val Expr: P0 = {
      val If = {
        val Else = P( Semi.? ~ `else` ~ Expr )
        P( `if` ~! "(" ~ ExprCtx.Expr ~ ")" ~ Expr ~ Else.? )
      }
      val While = P( `while` ~! "(" ~ Expr ~ ")" ~ Expr )
      val Try = {
        val Catch = P( `catch` ~! Expr )
        val Finally = P( `finally` ~! Expr )
        P( `try` ~! Expr ~ Catch.? ~ Finally.? )
      }
      val DoWhile = P( `do` ~! Expr ~ Semi.? ~ `while` ~ "(" ~ Expr ~ ")" )

      val For = {
        val Body = P( "(" ~! ExprCtx.Enumerators ~ ")" | "{" ~! StatCtx.Enumerators ~ "}" )
        P( `for` ~! Body ~ `yield`.? ~ Expr )
      }
      val Throw = P( `throw` ~! Expr )
      val Return = P( `return` ~! Expr.? )
      val LambdaRhs = if (curlyBlock) P( BlockStat ) else P( Expr )


      val ImplicitLambda = P( `implicit` ~ (Id | `_`) ~ (`:` ~ InfixType).? ~ `=>` ~ LambdaRhs.? )
      val ParenedLambda = P( Parened ~ (`=>` ~ LambdaRhs.? | ExprSuffix ~ PostfixSuffix) )
      val PostfixLambda = P( PostfixExpr ~ (`=>` ~ LambdaRhs.?).? )
      val SmallerExprOrLambda = P( ParenedLambda | PostfixLambda )
      P(
        If | While | Try | DoWhile | For | Throw | Return |
        ImplicitLambda | SmallerExprOrLambda
      )
    }
    val AscriptionType = if (curlyBlock) P( InfixType ) else P( Type )
    val Ascription = P( `:` ~! (`_*` |  AscriptionType | Annot.rep1) )
    val MatchAscriptionSuffix = P(`match` ~! "{" ~ CaseClauses ~ "}" | Ascription)
    val ExprPrefix = P( WL ~ CharIn("-+~!") ~ WS ~ !syntax.Basic.OpChar )
    val ExprSuffix = P( ("." ~! Id | TypeArgs | NoSemis ~ ArgList).rep ~ (NoSemis  ~ `_`).? )
    val PrefixExpr = P( ExprPrefix.? ~ SimpleExpr )
    val InfixSuffix = P( NoSemis ~ Id ~ TypeArgs.? ~ OneSemiMax ~ PrefixExpr ~ ExprSuffix)
    val PostFix = P( NotNewline ~ Id ~ Newline.? )
    val PostfixSuffix = P( InfixSuffix.rep ~ PostFix.? ~ (`=` ~! Expr).? ~ MatchAscriptionSuffix.?)

    val PostfixExpr: P0 = P( PrefixExpr ~ ExprSuffix ~ PostfixSuffix )

    val Parened = P ( "(" ~! Exprs.? ~ ")" )
    val SimpleExpr: P0 = {
      val Path = P( (Id ~ ".").rep ~ `this` ~ ("." ~! Id).rep | StableId )
      val New = P( `new` ~! AnonTmpl )

      P( XmlExpr | New | BlockExpr | ExprLiteral | Path | `_` | Parened )
    }
    val Guard : P0 = P( `if` ~! PostfixExpr )
  }
  val SimplePattern: P0 = {
    val ExtractorArgs = P( Pattern.rep("," ~! Pass) )
    val TupleEx = P( "(" ~! ExtractorArgs ~ ")" )
    val Extractor = P( StableId ~ TypeArgs.? ~ TupleEx.? )
    val Thingy = P( `_` ~ (`:` ~! TypePat).? ~ !"*" )
    P( XmlPattern | Thingy | PatLiteral | TupleEx | Extractor | VarId)
  }

  val BlockExpr: P0 = P( "{" ~! (CaseClauses | Block) ~ `}` )

  val BlockStat = {
    val Prelude = P( Annot.rep ~ `implicit`.? ~ `lazy`.? ~ LocalMod.rep )
    val Tmpl = P( Prelude ~ BlockDef )
    P( Import | Tmpl | StatCtx.Expr )
  }

  val Block: P0 = {
    val BlockEnd = P( Semis.? ~ &("}" | `case`) )
    val Body = P( BlockStat.rep(Semis) )
    P( Semis.? ~ Body ~! BlockEnd )
  }

  val Patterns: P0 = P( Pattern.rep1("," ~! Pass) )
  val Pattern: P0 = P( TypeOrBindPattern.rep1("|" ~! Pass) )
  val TypePattern = P( (`_` | VarId) ~ `:` ~ TypePat )
  val TypeOrBindPattern: P0 = P( TypePattern | BindPattern )
  val BindPattern: P0 = {
    val InfixPattern = P( `_*` | SimplePattern ~ (Id ~ SimplePattern).rep )
    val Binding = P( (VarId | `_`) ~ `@` )
    P( Binding ~ InfixPattern | InfixPattern | VarId )
  }

  val TypePat = P( CompoundType )
  val ParenArgList = P( "(" ~! (Exprs ~ (`:` ~! `_*`).?).? ~ ")" )
  val ArgList: P0 = P( ParenArgList | OneNLMax ~ BlockExpr )

  val CaseClauses: P0 = {
    // Need to lookahead for `class` and `object` because
    // the block { case object X } is not a case clause!
    val CaseClause: P0 = P( `case` ~ !(`class` | `object`) ~! Pattern ~ ExprCtx.Guard.? ~ `=>` ~ Block )
    P( CaseClause.rep1 )
  }
}
