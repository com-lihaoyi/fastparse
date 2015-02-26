package scalaParser
import acyclic.file

/**
 * Created by haoyi on 11/30/14.
 */
trait Exprs extends Core with Types with Xml{

  private implicit def wspStr(s: String) = rule( WL ~ str(s) )
  private implicit def wspCh(s: Char) = rule( WL ~ ch(s) )

  def NewBody: R0
  def BlockDef: R0

  def Import: R0 = {
    def ImportExpr: R0 = rule( StableId ~ ('.' ~ (`_` | Selectors)).? )
    def Selectors: R0 = rule( '{' ~ (Selector ~ ',').* ~ (Selector | `_`) ~ "}" )
    def Selector: R0 = rule( Id ~ (`=>` ~ (Id | `_`)).? )
    rule( `import` ~ ImportExpr.+(',') )
  }

  def Ascription = rule( `:` ~ (`_*` |  Type | Annot.+) )

  def LambdaHead: R0 = {
    def Binding = rule( (Id | `_`) ~ (`:` ~ Type).? )
    def Bindings = rule( '(' ~ Binding.*(',') ~ ')' )
    def Implicit = rule( `implicit`.? ~ Id ~ (`:` ~ InfixType).? )
    rule( (Bindings | Implicit | `_` ~ Ascription.?) ~ `=>` )
  }
  object StatCtx extends WsCtx(true)
  object ExprCtx extends WsCtx(false)
  def TypeExpr = ExprCtx.Expr
  class WsCtx(injectSemicolons: Boolean){

    def OneSemiMax = if (injectSemicolons) OneNLMax else MATCH
    def NoSemis = if (injectSemicolons) NotNewline else MATCH

    def Enumerators = {
      def Generator = rule( Pat1 ~ `<-` ~ Expr ~ Guard.? )
      def Assign = rule( Pat1 ~ `=` ~ Expr )
      def Enumerator = rule( Semis ~ Generator | optional(Semis) ~ Guard | Semis ~ Assign )
      rule( Generator ~ Enumerator.* ~ WL )
    }
    def Expr: R0 = {
      def If = {
        def Else = rule( Semi.? ~ `else` ~ Expr )
        rule( `if` ~ '(' ~ ExprCtx.Expr ~ ')' ~ Expr ~ Else.? )
      }
      def While = rule( `while` ~ '(' ~ Expr ~ ')' ~ Expr )
      def Try = {
        def Catch = rule( `catch` ~ Expr )
        def Finally = rule( `finally` ~ Expr )
        rule( `try` ~ Expr ~ Catch.? ~ Finally.? )
      }
      def DoWhile = rule( `do` ~ Expr ~ Semi.? ~ `while` ~ '(' ~ Expr ~ ")" )

      def For = {
        def Body = rule( '(' ~ ExprCtx.Enumerators ~ ')' | '{' ~ StatCtx.Enumerators ~ '}' )
        rule( `for` ~ Body ~ `yield`.? ~ Expr )
      }
      def Throw = rule( `throw` ~ Expr )
      def Return = rule( `return` ~ Expr.? )

      def SmallerExpr = rule( PostfixExpr ~ (`match` ~ '{' ~ CaseClauses ~ "}" | Ascription).? )
      def LambdaBody = rule( If | While | Try | DoWhile | For | Throw | Return | SmallerExpr )
      rule( LambdaHead.* ~ LambdaBody )
    }

    def PostfixExpr: R0 = {
      def Prefixed = rule( (WL ~ anyOf("-+~!") ~ WS ~ !Basic.OpChar) ~  SimpleExpr )
      def Assign = rule( SimpleExpr ~ (`=` ~ Expr).? )
      def PrefixExpr = rule( Prefixed | Assign )

      def InfixExpr = rule( PrefixExpr ~ (NoSemis ~ Id ~ TypeArgs.? ~ OneSemiMax ~ PrefixExpr).* )
      rule( InfixExpr ~ (NotNewline ~ Id ~ Newline.?).? )
    }

    def SimpleExpr: R0 = {
      def Path = rule( (Id ~ '.').* ~ `this` ~ ('.' ~ Id).* | StableId )
      def New = rule( `new` ~ NewBody )
      def Parened = rule ( '(' ~ Exprs.? ~ ")"  )
      def SimpleExpr1 = rule( XmlExpr | New | BlockExpr | Literal | Path | `_` | Parened)
      rule( SimpleExpr1 ~ ('.' ~ Id | TypeArgs | NoSemis ~ ArgList).* ~ (NoSemis  ~ `_`).?)
    }
    def Guard : R0 = rule( `if` ~ PostfixExpr )
  }
  def SimplePat: R0 = {
    def ExtractorArgs = rule( Pat.*(',') )
    def Extractor = rule( StableId ~ ('(' ~ ExtractorArgs ~ ')').? )
    def TupleEx = rule( '(' ~ ExtractorArgs.? ~ ')' )
    def Thingy = rule( `_` ~ (`:` ~ TypePat).? ~ !"*" )
    rule( XmlPattern | Thingy | Literal | TupleEx | Extractor | VarId)
  }

  def BlockExpr: R0 = rule( '{' ~ (CaseClauses | Block) ~ `}` )

  def BlockStats: R0 = {
    def Prelude = rule( Annot.* ~ `implicit`.? ~ `lazy`.? ~ LocalMod.* )
    def Tmpl = rule( Prelude ~ BlockDef )
    def BlockStat = rule( Import | Tmpl | StatCtx.Expr )
    rule( BlockStat.+(Semis) )
  }

  def Block: R0 = {
    def End = rule( Semis.? ~ &("}" | `case`) )
    def ResultExpr = rule{ StatCtx.Expr | LambdaHead ~ Block}
    def Body = rule( ResultExpr ~ End | BlockStats ~ (Semis ~ ResultExpr).? ~ End | End )
    rule( LambdaHead.* ~ Semis.? ~ Body )
  }

  def Patterns: R0 = rule( Pat.+(",") )
  def Pat: R0 = rule( Pat1.+('|') )
  def Pat1: R0 = rule( `_` ~ `:` ~ TypePat | VarId ~ `:` ~ TypePat | Pat2 )
  def Pat2: R0 = {
    def Pat3 = rule( `_*` | SimplePat ~ (Id ~ SimplePat).* )
    rule( (VarId | `_`) ~ `@` ~ Pat3 | Pat3 | VarId )
  }

  def TypePat = rule( CompoundType )

  def ArgList: R0 = rule( '(' ~ (Exprs ~ (`:` ~ `_*`).?).? ~ ")" | OneNLMax ~ BlockExpr )

  def CaseClauses: R0 = {
    def CaseClause: R0 = rule( `case` ~ Pat ~ ExprCtx.Guard.? ~ `=>` ~ Block )
    rule( CaseClause.+ )
  }
}
