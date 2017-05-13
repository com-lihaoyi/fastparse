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
    val Selectors: P0 = P( "{" ~/ Selector.repTC() ~ "}" )
    val ImportExpr: P0 = P( StableId ~ ("." ~/ (`_` | Selectors)).? )
    P( `import` ~/ ImportExpr.rep(1, sep = ",".~/) )
  }

  // Depending on where an expression is located, subtle behavior around
  // semicolon inference and arrow-type-ascriptions like i: a => b
  // varies.

  // Expressions used as statements, directly within a {block}
  object StatCtx extends WsCtx(semiInference=true, arrowTypeAscriptions=false)
  // Expressions nested within other expressions
  object ExprCtx extends WsCtx(semiInference=false, arrowTypeAscriptions=true)
  // Expressions directly within a `val x = ...` or `def x = ...`
  object FreeCtx extends WsCtx(semiInference=true, arrowTypeAscriptions=true)

  val TypeExpr = ExprCtx.Expr

  class WsCtx(semiInference: Boolean, arrowTypeAscriptions: Boolean){

    val OneSemiMax = if (semiInference) OneNLMax else Pass
    val NoSemis = if (semiInference) NotNewline else Pass


    val Enumerators = {
      val Generator = P( `<-` ~/ Expr ~ Guard.? )
      val Assign = P( `=` ~/ Expr )
      // CuttingSemis is a bit weird, and unlike other places within this parser
      // which just use `Semis`. This is because unlike other semicolons, semis
      // within a generator cannot be trailing: any set of semis *must* be followed
      // by another generator, assignment or guard! Thus we need to make sure we
      val CuttingSemis = P( WL ~~ ";".~/.? ~~ WL )
      val GenAssign = P( `val`.? ~ TypeOrBindPattern ~/ (Generator | Assign) )
      val Enumerator = P( CuttingSemis ~~ (GenAssign | Guard) | Guard  )
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
      val LambdaRhs = if (semiInference) P( BlockChunk ) else P( Expr )


      val ImplicitLambda = P( `implicit` ~ (Id | `_`) ~ (`:` ~ InfixType).? ~ `=>` ~ LambdaRhs.? )
      val ParenedLambda = P( Parened ~~ (WL ~ `=>` ~ LambdaRhs.? | ExprSuffix ~~ PostfixSuffix ~ SuperPostfixSuffix) )
      val PostfixLambda = P( PostfixExpr ~ (`=>` ~ LambdaRhs.? | SuperPostfixSuffix).? )
      val SmallerExprOrLambda = P( ParenedLambda | PostfixLambda )
      P(
        If | While | Try | DoWhile | For | Throw | Return |
        ImplicitLambda | SmallerExprOrLambda
      )
    }
    val SuperPostfixSuffix = P( (`=` ~/ Expr).? ~ MatchAscriptionSuffix.? )
    val AscriptionType = if (arrowTypeAscriptions) P( Type ) else P( InfixType )
    val Ascription = P( `:` ~/ (`_*` |  AscriptionType | Annot.rep(1)) )
    val MatchAscriptionSuffix = P(`match` ~/ "{" ~ CaseClauses | Ascription)
    val ExprPrefix = P( WL ~ CharIn("-+!~") ~~ !syntax.Basic.OpChar ~ WS)
    val ExprSuffix = P( (WL ~ "." ~/ Id | WL ~ TypeArgs | NoSemis ~ ArgList).repX ~~ (NoSemis  ~ `_`).? )
    val PrefixExpr = P( ExprPrefix.? ~ SimpleExpr )

    // Intermediate `WL` needs to always be non-cutting, because you need to
    // backtrack out of `InfixSuffix` into `PostFixSuffix` if it doesn't work out
    val InfixSuffix = P( NoSemis ~~ WL ~~ Id ~ TypeArgs.? ~~ OneSemiMax ~ PrefixExpr ~~ ExprSuffix)
    val PostFix = P( NoSemis ~~ WL ~~ Id ~ Newline.? )

    val PostfixSuffix = P( InfixSuffix.repX ~~ PostFix.?)

    val PostfixExpr: P0 = P( PrefixExpr ~~ ExprSuffix ~~ PostfixSuffix )

    val Parened = P ( "(" ~/ TypeExpr.repTC() ~ ")" )
    val SimpleExpr: P0 = {
      val New = P( `new` ~/ AnonTmpl )

      P( XmlExpr | New | BlockExpr | ExprLiteral | StableId | `_` | Parened )
    }
    val Guard : P0 = P( `if` ~/ PostfixExpr )
  }
  val SimplePattern: P0 = {
    val TupleEx = P( "(" ~/ Pattern.repTC() ~ ")" )
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

  def BaseBlock(end: P0)(implicit name: sourcecode.Name): P0 = {
    val BlockEnd = P( Semis.? ~ &(end) )
    val Body = P( BlockChunk.repX(sep = Semis) )
    P( Semis.? ~ BlockLambda.? ~ Body ~/ BlockEnd )
  }
  val Block = BaseBlock("}")
  val CaseBlock = BaseBlock("}" | `case`)

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
  val ParenArgList = P( "(" ~/ (Exprs ~ (`:` ~/ `_*`).?).? ~ TrailingComma ~ ")" )
  val ArgList: P0 = P( ParenArgList | OneNLMax ~ BlockExpr )

  val CaseClauses: P0 = {
    // Need to lookahead for `class` and `object` because
    // the block { case object X } is not a case clause!
    val CaseClause: P0 = P( `case` ~ !(`class` | `object`) ~/ Pattern ~ ExprCtx.Guard.? ~ `=>` ~ CaseBlock  )
    P( CaseClause.rep(1) ~ "}"  )
  }
}
