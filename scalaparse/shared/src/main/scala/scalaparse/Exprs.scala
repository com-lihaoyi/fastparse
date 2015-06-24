package scalaparse

import acyclic.file
import fastparse._

import scalaparse.syntax.Identifiers

trait Exprs extends Core with Types with Xml{
  private implicit def parserApi[T, V](p0: T)
                                      (implicit c: T => P[V]) =
    new ParserApiImpl2[V](p0, WL)

  def AnonTmpl: P0
  def BlockDef: P0

  val Import: P0 = {
    val Selector: P0 = P( (Id | `_`) ~ (`=>` ~! (Id | `_`)).? )
    val Selectors: P0 = P( "{" ~! Selector.rep(sep = "," ~!) ~ "}" )
    val ImportExpr: P0 = P( StableId ~ ("." ~! (`_` | Selectors)).? )
    P( `import` ~! ImportExpr.rep(1, sep = "," ~!) )
  }

  object StatCtx extends WsCtx(curlyBlock=true)
  object ExprCtx extends WsCtx(curlyBlock=false)

  val TypeExpr = ExprCtx.Expr

  class WsCtx(curlyBlock: Boolean){

    val OneSemiMax = if (curlyBlock) OneNLMax else Pass
    val NoSemis = if (curlyBlock) NotNewline else Pass


    def Enumerators(end: P0) = {
      val Generator = P( `<-` ~! Expr ~ Guard.? )
      val Assign = P( `=` ~! Expr )
      val Enumerator = P( Semis ~ `val`.? ~ TypeOrBindPattern ~! (Generator | Assign) | Semis.? ~ Guard  )
      P( TypeOrBindPattern ~ Generator ~~ Enumerator.rep ~ end )
    }

    val Expr: P0 = {
      val If = {
        val Else = P( Semi.? ~ `else` ~! Expr )
        P( `if` ~! "(" ~ ExprCtx.Expr ~ ")" ~ Expr ~ Else.? )
      }
      val While = P( `while` ~! "(" ~ ExprCtx.Expr ~ ")" ~ Expr )
      val Try = {
        val Catch = P( `catch` ~! Expr )
        val Finally = P( `finally` ~! Expr )
        P( `try` ~! Expr ~ Catch.? ~ Finally.? )
      }
      val DoWhile = P( `do` ~! Expr ~ Semi.? ~ `while` ~ "(" ~ ExprCtx.Expr ~ ")" )

      val For = {
        val Body = P( "(" ~! ExprCtx.Enumerators(")") | "{" ~! StatCtx.Enumerators("}") )
        P( `for` ~! Body ~ `yield`.? ~ Expr )
      }
      val Throw = P( `throw` ~! Expr )
      val Return = P( `return` ~! Expr.? )
      val LambdaRhs = if (curlyBlock) P( BlockStat ) else P( Expr )


      val ImplicitLambda = P( `implicit` ~ (Id | `_`) ~ (`:` ~ InfixType).? ~ `=>` ~ LambdaRhs.? )
      val ParenedLambda = P( Parened ~~ (WL ~ `=>` ~ LambdaRhs.? | ExprSuffix ~~ PostfixSuffix) )
      val PostfixLambda = P( PostfixExpr ~ (`=>` ~ LambdaRhs.?).? )
      val SmallerExprOrLambda = P( ParenedLambda | PostfixLambda )
      P(
        If | While | Try | DoWhile | For | Throw | Return |
        ImplicitLambda | SmallerExprOrLambda
      )
    }
    val AscriptionType = if (curlyBlock) P( PostfixType ) else P( Type )
    val Ascription = P( `:` ~! (`_*` |  AscriptionType | Annot.rep(1)) )
    val MatchAscriptionSuffix = P(`match` ~! "{" ~ CaseClauses | Ascription)
    val ExprPrefix = P( WL ~ CharIn("-+~!") ~~ !syntax.Basic.OpChar ~ WS)
    val ExprSuffix = P( (WL ~ "." ~! Id | WL ~ TypeArgs | NoSemis ~ ArgList).rep ~~ (NoSemis  ~ `_`).? )
    val PrefixExpr = P( ExprPrefix.? ~ SimpleExpr )
    val InfixSuffix = P( NoSemis ~ Id ~ TypeArgs.? ~~ OneSemiMax ~ PrefixExpr ~~ ExprSuffix)
    val PostFix = P( NoSemis ~ Id ~ Newline.? )
    val PostfixSuffix = P( InfixSuffix.rep ~~ PostFix.? ~ (`=` ~! Expr).? ~ MatchAscriptionSuffix.?)

    val PostfixExpr: P0 = P( PrefixExpr ~~ ExprSuffix ~~ PostfixSuffix )

    val Parened = P ( "(" ~! TypeExpr.rep(0, "," ~!) ~ ")" )
    val SimpleExpr: P0 = {
      val Path = P( (Id ~ ".").rep ~ `this` ~ ("." ~! Id).rep | StableId )
      val New = P( `new` ~! AnonTmpl )

      P( XmlExpr | New | BlockExpr | ExprLiteral | Path | `_` | Parened )
    }
    val Guard : P0 = P( `if` ~! PostfixExpr )
  }
  val SimplePattern: P0 = {
    val TupleEx = P( "(" ~! Pattern.rep(sep = "," ~!) ~ ")" )
    val Extractor = P( StableId ~ TypeArgs.? ~ TupleEx.? )
    val Thingy = P( `_` ~ (`:` ~! TypePat).? ~ !("*" ~~ !syntax.Basic.OpChar) )
    P( XmlPattern | Thingy | PatLiteral | TupleEx | Extractor | VarId)
  }

  val BlockExpr: P0 = P( "{" ~! (CaseClauses | Block ~ "}") )

  val BlockLambdaHead: P0 = P( "(" ~ BlockLambdaHead ~ ")" | `this` | Id | `_` )
  val BlockLambda = P( BlockLambdaHead  ~ (`=>` | `:` ~ InfixType ~ `=>`.?) )
//      val BlockLambda = Pass

  val BlockStat = {
    val Prelude = P( Annot.rep ~ `implicit`.? ~ `lazy`.? ~ LocalMod.rep )
    val Tmpl = P( Prelude ~ BlockDef )
    P( BlockLambda ~ (Import | Tmpl | StatCtx.Expr).? | BlockLambda.? ~ (Import | Tmpl | StatCtx.Expr) )
  }

  val Block: P0 = {
    val BlockEnd = P( Semis.? ~ &("}" | `case`) )
    val Body = P( BlockStat.repX(sep = Semis) )
    P( Semis.? ~ BlockLambda.? ~ Body ~! BlockEnd )
  }

  val Patterns: P0 = P( Pattern.rep(1, sep = "," ~!) )
  val Pattern: P0 = P( (WL ~ TypeOrBindPattern).rep(1, sep = "|" ~!) )
  val TypePattern = P( (`_` | VarId) ~ `:` ~ TypePat )
  val TypeOrBindPattern: P0 = P( TypePattern | BindPattern )
  val BindPattern: P0 = {
    val InfixPattern = P( SimplePattern ~ (Id ~! SimplePattern).rep | `_*` )
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
    P( CaseClause.rep(1) ~ "}"  )
  }
}
