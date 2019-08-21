package scalaparse

import fastparse._, ScalaWhitespace._
trait Exprs extends Core with Types with Xml{
  def AnonTmpl[$: P]: P[Unit]
  def BlockDef[$: P]: P[Unit]

  def Import[$: P]: P[Unit] = {
    def Selector: P[Unit] = P( (Id | _wildcard) ~ (`=>` ~/ (Id | _wildcard)).? )
    def Selectors: P[Unit] = P( "{" ~/ Selector.repTC() ~ "}" )
    def ImportExpr: P[Unit] = P( StableId ~ ("." ~/ (_wildcard | Selectors)).? )
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

  def TypeExpr[$: P] = ExprCtx.Expr

  class WsCtx(semiInference: Boolean, arrowTypeAscriptions: Boolean){

    def OneSemiMax[$: P] = if (semiInference) OneNLMax else Pass
    def NoSemis[$: P] = if (semiInference) NotNewline else Pass


    def Enumerators[$: P] = {
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

    def Expr[$: P]: P[Unit] = {
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


      def ImplicitLambda = P( `implicit` ~ (Id | _wildcard) ~ (`:` ~ InfixType).? ~ `=>` ~ LambdaRhs.? )
      def ParenedLambda = P( Parened ~~ (WL ~ `=>` ~ LambdaRhs.? | ExprSuffix ~~ PostfixSuffix ~ SuperPostfixSuffix) )
      def PostfixLambda = P( PostfixExpr ~ (`=>` ~ LambdaRhs.? | SuperPostfixSuffix).? )
      def SmallerExprOrLambda = P( ParenedLambda | PostfixLambda )
      P(
        If | While | Try | DoWhile | For | Throw | Return |
        ImplicitLambda | SmallerExprOrLambda
      )
    }

    def SuperPostfixSuffix[$: P] = P( (`=` ~/ Expr).? ~ MatchAscriptionSuffix.? )
    def AscriptionType[$: P]  = if (arrowTypeAscriptions) P( Type ) else P( InfixType )
    def Ascription[$: P] = P( `:` ~/ (`_*` |  AscriptionType | Annot.rep(1)) )
    def MatchAscriptionSuffix[$: P] = P(`match` ~/ "{" ~ CaseClauses | Ascription)
    def ExprPrefix[$: P] = P( WL ~ CharIn("\\-+!~") ~~ !syntax.Basic.OpChar ~ WS)
    def ExprSuffix[$: P] = P( (WL ~ "." ~/ Id | WL ~ TypeArgs | NoSemis ~ ArgList).repX ~~ (NoSemis  ~ _wildcard).? )
    def PrefixExpr[$: P] = P( ExprPrefix.? ~ SimpleExpr )

    // Intermediate `WL` needs to always be non-cutting, because you need to
    // backtrack out of `InfixSuffix` into `PostFixSuffix` if it doesn't work out
    def InfixSuffix[$: P] = P( NoSemis ~~ WL ~~ Id ~ TypeArgs.? ~~ OneSemiMax ~ PrefixExpr ~~ ExprSuffix)
    def PostFix[$: P] = P( NoSemis ~~ WL ~~ Id ~ Newline.? )

    def PostfixSuffix[$: P] = P( InfixSuffix.repX ~~ PostFix.?)

    def PostfixExpr[$: P]: P[Unit] = P( PrefixExpr ~~ ExprSuffix ~~ PostfixSuffix )

    def Parened[$: P] = P ( "(" ~/ TypeExpr.repTC() ~ ")" )
    def SimpleExpr[$: P]: P[Unit] = {
      def New = P( `new` ~/ AnonTmpl )

      P( XmlExpr | New | BlockExpr | ExprLiteral | StableId | _wildcard | Parened )
    }
    def Guard[$: P] : P[Unit] = P( `if` ~/ PostfixExpr )
  }

  def SimplePattern[$: P]: P[Unit] = {
    def TupleEx = P( "(" ~/ Pattern.repTC() ~ ")" )
    def Extractor = P( StableId ~ TypeArgs.? ~ TupleEx.? )
    def Thingy = P( _wildcard ~ (`:` ~/ TypePat).? ~ !("*" ~~ !syntax.Basic.OpChar) )
    P( XmlPattern | Thingy | PatLiteral | TupleEx | Extractor | VarId )
  }

  def BlockExpr[$: P]: P[Unit] = P( "{" ~/ (CaseClauses | Block ~ "}") )

  def BlockLambdaHead[$: P]: P[Unit] = P( "(" ~ BlockLambdaHead ~ ")" | `this` | Id | _wildcard )

  def BlockLambda[$: P] = P( BlockLambdaHead  ~ (`=>` | `:` ~ InfixType ~ `=>`.?) )

  def BlockChunk[$: P]  = {
    def Prelude = P( Annot.rep ~ LocalMod.rep )
    def BlockStat = P( Import | Prelude ~ BlockDef | StatCtx.Expr )
    P( BlockLambda.rep ~ BlockStat.rep(0, Semis, Int.MaxValue, -1) )
  }

  def BaseBlock[$: P](end: => P[Unit])(implicit name: sourcecode.Name): P[Unit] = {
    def BlockEnd = P( Semis.? ~ &(end) )
    def Body = P( BlockChunk.repX(0, Semis, Int.MaxValue, -1) )
    P( Semis.? ~ BlockLambda.? ~ Body ~/ BlockEnd )
  }

  def Block[$: P]  = BaseBlock("}")
  def CaseBlock[$: P]  = BaseBlock("}" | `case`)

  def Patterns[$: P]: P[Unit] = P( Pattern.rep(1, sep = ","./) )
  def Pattern[$: P]: P[Unit] = P( (WL ~ TypeOrBindPattern).rep(1, sep = "|"./) )
  def TypePattern[$: P] = P( (_wildcard | BacktickId | VarId) ~ `:` ~ TypePat )
  def TypeOrBindPattern[$: P]: P[Unit] = P( TypePattern | BindPattern )
  def BindPattern[$: P]: P[Unit] = {
    def InfixPattern = P( SimplePattern ~ (Id ~/ SimplePattern).rep | `_*` )
    def Binding = P( (Id | _wildcard) ~ `@` )
    P( Binding ~ InfixPattern | InfixPattern | VarId )
  }

  def TypePat[$: P] = P( CompoundType )
  def ParenArgList[$: P] = P( "(" ~/ (Exprs ~ (`:` ~/ `_*`).?).? ~ TrailingComma ~ ")" )
  def ArgList[$: P]: P[Unit] = P( ParenArgList | OneNLMax ~ BlockExpr )

  def CaseClauses[$: P]: P[Unit] = {
    // Need to lookahead for `class` and `object` because
    // the block { case object X } is not a case clause!
    def CaseClause: P[Unit] = P( `case` ~ !(`class` | `object`) ~/ Pattern ~ ExprCtx.Guard.? ~ `=>` ~ CaseBlock  )
    P( CaseClause.rep(1) ~ "}"  )
  }
}
