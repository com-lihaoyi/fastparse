package scalaparse

import fastparse._, ScalaWhitespace._
trait Exprs extends Core with Types with Xml{
  def AnonTmpl[_: P]: P[Unit]
  def BlockDef[_: P]: P[Unit]

  def Import[_: P]: P[Unit] = {
    def Selector: P[Unit] = P( (Id | `_`) ~ (`=>` ~/ (Id | `_`)).? )
    def Selectors: P[Unit] = P( "{" ~/ Selector.repTC() ~ "}" )
    def ImportExpr: P[Unit] = P( StableId ~ ("." ~/ (`_` | Selectors)).? )
    P( `import` ~/ ImportExpr.rep(1, sep = ","./) )
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

  def TypeExpr[_: P] = ExprCtx.Expr

  class WsCtx(semiInference: Boolean, arrowTypeAscriptions: Boolean){

    def OneSemiMax[_: P] = if (semiInference) OneNLMax else Pass
    def NoSemis[_: P] = if (semiInference) NotNewline else Pass


    def Enumerators[_: P] = {
      def Generator = P( `<-` ~/ Expr ~ Guard.? )
      def Assign = P( `=` ~/ Expr )
      // CuttingSemis is a bit weird, and unlike other places within this parser
      // which just use `Semis`. This is because unlike other semicolons, semis
      // within a generator cannot be trailing: any set of semis *must* be followed
      // by another generator, assignment or guard! Thus we need to make sure we
      def CuttingSemis = P( WL ~~ ";"./.? ~~ WL )
      def GenAssign = P( `val`.? ~ TypeOrBindPattern ~/ (Generator | Assign) )
      def Enumerator = P( CuttingSemis ~~ (GenAssign | Guard) | Guard  )
      P( TypeOrBindPattern ~ Generator ~~ Enumerator.repX )
    }

    def Expr[_: P]: P[Unit] = {
      def If = {
        def Else = P( Semis.? ~ `else` ~/ Expr )
        P( `if` ~/ "(" ~ ExprCtx.Expr ~ ")" ~ Expr ~ Else.? )
      }
      def While = P( `while` ~/ "(" ~ ExprCtx.Expr ~ ")" ~ Expr )
      def Try = {
        def Catch = P( `catch` ~/ Expr )
        def Finally = P( `finally` ~/ Expr )
        P( `try` ~/ Expr ~ Catch.? ~ Finally.? )
      }
      def DoWhile = P( `do` ~/ Expr ~ Semis.? ~ `while` ~ "(" ~ ExprCtx.Expr ~ ")" )

      def For = {
        def Body= P( "(" ~/ ExprCtx.Enumerators ~ ")" | "{" ~/ StatCtx.Enumerators ~ "}" )
        P( `for` ~/ Body ~ `yield`.? ~ Expr )
      }
      def Throw = P( `throw` ~/ Expr )
      def Return = P( `return` ~/ Expr.? )
      def LambdaRhs = if (semiInference) P( BlockChunk ) else P( Expr )


      def ImplicitLambda = P( `implicit` ~ (Id | `_`) ~ (`:` ~ InfixType).? ~ `=>` ~ LambdaRhs.? )
      def ParenedLambda = P( Parened ~~ (WL ~ `=>` ~ LambdaRhs.? | ExprSuffix ~~ PostfixSuffix ~ SuperPostfixSuffix) )
      def PostfixLambda = P( PostfixExpr ~ (`=>` ~ LambdaRhs.? | SuperPostfixSuffix).? )
      def SmallerExprOrLambda = P( ParenedLambda | PostfixLambda )
      P(
        If | While | Try | DoWhile | For | Throw | Return |
        ImplicitLambda | SmallerExprOrLambda
      )
    }

    def SuperPostfixSuffix[_: P] = P( (`=` ~/ Expr).? ~ MatchAscriptionSuffix.? )
    def AscriptionType[_: P]  = if (arrowTypeAscriptions) P( Type ) else P( InfixType )
    def Ascription[_: P] = P( `:` ~/ (`_*` |  AscriptionType | Annot.rep(1)) )
    def MatchAscriptionSuffix[_: P] = P(`match` ~/ "{" ~ CaseClauses | Ascription)
    def ExprPrefix[_: P] = P( WL ~ CharIn("\\-+!~") ~~ !syntax.Basic.OpChar ~ WS)
    def ExprSuffix[_: P] = P( (WL ~ "." ~/ Id | WL ~ TypeArgs | NoSemis ~ ArgList).repX ~~ (NoSemis  ~ `_`).? )
    def PrefixExpr[_: P] = P( ExprPrefix.? ~ SimpleExpr )

    // Intermediate `WL` needs to always be non-cutting, because you need to
    // backtrack out of `InfixSuffix` into `PostFixSuffix` if it doesn't work out
    def InfixSuffix[_: P] = P( NoSemis ~~ WL ~~ Id ~ TypeArgs.? ~~ OneSemiMax ~ PrefixExpr ~~ ExprSuffix)
    def PostFix[_: P] = P( NoSemis ~~ WL ~~ Id ~ Newline.? )

    def PostfixSuffix[_: P] = P( InfixSuffix.repX ~~ PostFix.?)

    def PostfixExpr[_: P]: P[Unit] = P( PrefixExpr ~~ ExprSuffix ~~ PostfixSuffix )

    def Parened[_: P] = P ( "(" ~/ TypeExpr.repTC() ~ ")" )
    def SimpleExpr[_: P]: P[Unit] = {
      def New = P( `new` ~/ AnonTmpl )

      P( XmlExpr | New | BlockExpr | ExprLiteral | StableId | `_` | Parened )
    }
    def Guard[_: P] : P[Unit] = P( `if` ~/ PostfixExpr )
  }

  def SimplePattern[_: P]: P[Unit] = {
    def TupleEx = P( "(" ~/ Pattern.repTC() ~ ")" )
    def Extractor = P( StableId ~ TypeArgs.? ~ TupleEx.? )
    def Thingy = P( `_` ~ (`:` ~/ TypePat).? ~ !("*" ~~ !syntax.Basic.OpChar) )
    P( XmlPattern | Thingy | PatLiteral | TupleEx | Extractor | VarId )
  }

  def BlockExpr[_: P]: P[Unit] = P( "{" ~/ (CaseClauses | Block ~ "}") )

  def BlockLambdaHead[_: P]: P[Unit] = P( "(" ~ BlockLambdaHead ~ ")" | `this` | Id | `_` )

  def BlockLambda[_: P] = P( BlockLambdaHead  ~ (`=>` | `:` ~ InfixType ~ `=>`.?) )

  def BlockChunk[_: P]  = {
    def Prelude = P( Annot.rep ~ LocalMod.rep )
    def BlockStat = P( Import | Prelude ~ BlockDef | StatCtx.Expr )
    P( BlockLambda.rep ~ BlockStat.rep(sep = Semis) )
  }

  def BaseBlock[_: P](end: => P[Unit])(implicit name: sourcecode.Name): P[Unit] = {
    def BlockEnd = P( Semis.? ~ &(end) )
    def Body = P( BlockChunk.repX(sep = Semis) )
    P( Semis.? ~ BlockLambda.? ~ Body ~/ BlockEnd )
  }

  def Block[_: P]  = BaseBlock("}")
  def CaseBlock[_: P]  = BaseBlock("}" | `case`)

  def Patterns[_: P]: P[Unit] = P( Pattern.rep(1, sep = ","./) )
  def Pattern[_: P]: P[Unit] = P( (WL ~ TypeOrBindPattern).rep(1, sep = "|"./) )
  def TypePattern[_: P] = P( (`_` | BacktickId | VarId) ~ `:` ~ TypePat )
  def TypeOrBindPattern[_: P]: P[Unit] = P( TypePattern | BindPattern )
  def BindPattern[_: P]: P[Unit] = {
    def InfixPattern = P( SimplePattern ~ (Id ~/ SimplePattern).rep | `_*` )
    def Binding = P( (Id | `_`) ~ `@` )
    P( Binding ~ InfixPattern | InfixPattern | VarId )
  }

  def TypePat[_: P] = P( CompoundType )
  def ParenArgList[_: P] = P( "(" ~/ (Exprs ~ (`:` ~/ `_*`).?).? ~ TrailingComma ~ ")" )
  def ArgList[_: P]: P[Unit] = P( ParenArgList | OneNLMax ~ BlockExpr )

  def CaseClauses[_: P]: P[Unit] = {
    // Need to lookahead for `class` and `object` because
    // the block { case object X } is not a case clause!
    def CaseClause: P[Unit] = P( `case` ~ !(`class` | `object`) ~/ Pattern ~ ExprCtx.Guard.? ~ `=>` ~ CaseBlock  )
    P( CaseClause.rep(1) ~ "}"  )
  }
}
