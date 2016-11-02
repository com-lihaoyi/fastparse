package scalaparse

import acyclic.file

import scalaparse.syntax.Identifiers
import fastparse.noApi._
trait Exprs extends Core with Types with Xml{

  import WhitespaceApi._
  def AnonTmpl: P0
  def BlockDef: P0

  val Import: P0 = {
    val Selector: P0 = P( (Id | `_`) ~ (`=>` ~/ (Id | `_`)).? )
    val Selectors: P0 = P( "{" ~/ Selector.rep(sep = ",".~/) ~ "}" )
    val ImportExpr: P0 = P( StableId ~ ("." ~/ (`_` | Selectors)).? )
    P( `import` ~/ ImportExpr.rep(1, sep = ",".~/) )
  }

  object StatCtx extends WsCtx(curlyBlock=true)
  object ExprCtx extends WsCtx(curlyBlock=false)

  val TypeExpr = ExprCtx.Expr

  class WsCtx(curlyBlock: Boolean){

    val OneSemiMax = if (curlyBlock) OneNLMax else Pass
    val NoSemis = if (curlyBlock) NotNewline else Pass


    val Enumerators = {
      val Generator = P( `<-` ~/ Expr ~ Guard.? )
      val Assign = P( `=` ~/ Expr )
      val Enumerator = P( Semis ~ `val`.? ~ TypeOrBindPattern ~/ (Generator | Assign) | Semis.? ~ Guard  )
      P( TypeOrBindPattern ~ Generator ~~ Enumerator.repX )
    }

    val Expr: P0 = {
      val If = {
        val Else = P( Semi.? ~ `else` ~/ Expr )
        P( `if` ~/ "(" ~ ExprCtx.Expr ~ ")" ~ Expr ~ Else.? )
      }
      val While = P( `while` ~/ "(" ~ ExprCtx.Expr ~ ")" ~ Expr )
      val Try = {
        val Catch = P( `catch` ~/ Expr )
        val Finally = P( `finally` ~/ Expr )
        P( `try` ~/ Expr ~ Catch.? ~ Finally.? )
      }
      val DoWhile = P( `do` ~/ Expr ~ Semi.? ~ `while` ~ "(" ~ ExprCtx.Expr ~ ")" )

      val For = {
        val Body = P( "(" ~/ ExprCtx.Enumerators ~ ")" | "{" ~/ StatCtx.Enumerators ~ "}" )
        P( `for` ~/ Body ~ `yield`.? ~ Expr )
      }
      val Throw = P( `throw` ~/ Expr )
      val Return = P( `return` ~/ Expr.? )
      val LambdaRhs = if (curlyBlock) P( BlockChunk ) else P( Expr )


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
    val Ascription = P( `:` ~/ (`_*` |  AscriptionType | Annot.rep(1)) )
    val MatchAscriptionSuffix = P(`match` ~/ "{" ~ CaseClauses | Ascription)
    val ExprPrefix = P( WL ~ CharIn("-+!~") ~~ !syntax.Basic.OpChar ~ WS)
    val ExprSuffix = P( (WL ~ "." ~/ Id | WL ~ TypeArgs | NoSemis ~ ArgList).repX ~~ (NoSemis  ~ `_`).? )
    val PrefixExpr = P( ExprPrefix.? ~ SimpleExpr )
    
    // Intermediate `WL` needs to always be non-cutting, because you need to
    // backtrack out of `InfixSuffix` into `PostFixSuffix` if it doesn't work out
    val InfixSuffix = P( NoSemis ~~ WL ~~ Id ~ TypeArgs.? ~~ OneSemiMax ~ PrefixExpr ~~ ExprSuffix)
    val PostFix = P( NoSemis ~~ WL ~~ Id ~ Newline.? )

    val PostfixSuffix = P( InfixSuffix.repX ~~ PostFix.? ~ (`=` ~/ Expr).? ~ MatchAscriptionSuffix.?)

    val PostfixExpr: P0 = P( PrefixExpr ~~ ExprSuffix ~~ PostfixSuffix )

    val Parened = P ( "(" ~/ TypeExpr.rep(0, ",".~/) ~ ")" )
    val SimpleExpr: P0 = {
      val New = P( `new` ~/ AnonTmpl )

      P( XmlExpr | New | BlockExpr | ExprLiteral | StableId | `_` | Parened )
    }
    val Guard : P0 = P( `if` ~/ PostfixExpr )
  }
  val SimplePattern: P0 = {
    val TupleEx = P( "(" ~/ Pattern.rep(sep = ",".~/) ~ ")" )
    val Extractor = P( StableId ~ TypeArgs.? ~ TupleEx.? )
    val Thingy = P( `_` ~ (`:` ~/ TypePat).? ~ !("*" ~~ !syntax.Basic.OpChar) )
    P( XmlPattern | Thingy | PatLiteral | TupleEx | Extractor | VarId)
  }

  val BlockExpr: P0 = P( "{" ~/ (CaseClauses | Block ~ "}") )

  val BlockLambdaHead: P0 = P( "(" ~ BlockLambdaHead ~ ")" | `this` | Id | `_` )
  val BlockLambda = P( BlockLambdaHead  ~ (`=>` | `:` ~ InfixType ~ `=>`.?) )

  val BlockChunk = {
    val Prelude = P( Annot.rep ~ `implicit`.? ~ `lazy`.? ~ LocalMod.rep )
    val BlockStat = P( Import | Prelude ~ BlockDef | StatCtx.Expr )
    P( BlockLambda.rep ~ BlockStat.rep(sep = Semis) )
  }

  val Block: P0 = {
    val BlockEnd = P( Semis.? ~ &("}" | `case`) )
    val Body = P( BlockChunk.repX(sep = Semis) )
    P( Semis.? ~ BlockLambda.? ~ Body ~/ BlockEnd )
  }

  val Patterns: P0 = P( Pattern.rep(1, sep = ",".~/) )
  val Pattern: P0 = P( (WL ~ TypeOrBindPattern).rep(1, sep = "|".~/) )
  val TypePattern = P( (`_` | BacktickId | VarId) ~ `:` ~ TypePat )
  val TypeOrBindPattern: P0 = P( TypePattern | BindPattern )
  val BindPattern: P0 = {
    val InfixPattern = P( SimplePattern ~ (Id ~/ SimplePattern).rep | `_*` )
    val Binding = P( (Id | `_`) ~ `@` )
    P( Binding ~ InfixPattern | InfixPattern | VarId )
  }

  val TypePat = P( CompoundType )
  val ParenArgList = P( "(" ~/ (Exprs ~ (`:` ~/ `_*`).?).? ~ ")" )
  val ArgList: P0 = P( ParenArgList | OneNLMax ~ BlockExpr )

  val CaseClauses: P0 = {
    // Need to lookahead for `class` and `object` because
    // the block { case object X } is not a case clause!
    val CaseClause: P0 = P( `case` ~ !(`class` | `object`) ~/ Pattern ~ ExprCtx.Guard.? ~ `=>` ~ Block )
    P( CaseClause.rep(1) ~ "}"  )
  }
}
