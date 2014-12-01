package scalaParser

import macros.Macros._

import acyclic.file

/**
 * Created by haoyi on 11/30/14.
 */
trait Exprs extends Core with Types with Xml{

  private implicit def wspStr(s: String) = rule( WL ~ str(s) )
  private implicit def wspCh(s: Char) = rule( WL ~ ch(s) )
  import KeyWordOperators._
  import KeyWordOperators.`_`

  def NewBody: R0
  def BlockDef: R0

  def Import: R0 = {
    def ImportExpr: R0 = rule( StableId ~ opt('.' ~ (`_` | Selectors)) )
    def Selectors: R0 = rule( '{' ~ rep(Selector ~ ',') ~ (Selector | `_`) ~ "}" )
    def Selector: R0 = rule( Id ~ opt(`=>` ~ (Id | `_`)) )
    rule( `import` ~ rep1Sep(ImportExpr, ',') )
  }

  def Ascription = rule( `:` ~ (`_*` |  Type | rep1(Annot)) )

  def LambdaHead: R0 = {
    def Binding = rule( (Id | `_`) ~ opt(`:` ~ Type) )
    def Bindings = rule( '(' ~ repSep(Binding, ',') ~ ')' )
    def Implicit = rule( opt(`implicit`) ~ Id ~ opt(`:` ~ InfixType) )
    rule( (Bindings | Implicit | `_` ~ opt(Ascription)) ~ `=>` )
  }

  def Expr = Expr0()

  def Expr0(G: Boolean = false): R0 = {
    def If = {
      def Else = rule( opt(Semi) ~ `else` ~ Expr0(G) )
      rule( `if` ~ '(' ~ Expr ~ ')' ~ Expr0(G) ~ opt(Else) )
    }
    def While = rule( `while` ~ '(' ~ Expr ~ ')' ~ Expr0(G) )
    def Try = {
      def Catch = rule( `catch` ~ Expr0(G) )
      def Finally = rule( `finally` ~ Expr0(G) )
      rule( `try` ~ Expr0(G) ~ opt(Catch) ~ opt(Finally) )
    }
    def DoWhile = rule( `do` ~ Expr0(G) ~ opt(Semi) ~ `while` ~ '(' ~ Expr ~ ")" )
    def Enumerators(G: Boolean) = {
      def Generator = rule( Pat1 ~ `<-` ~ Expr0(G) ~ opt(Guard(G)) )
      def Enumerator = rule( Semis ~ Generator | optional(Semis) ~ Guard(G) | Semis ~ Pat1 ~ `=` ~ Expr0(G) )
      rule( Generator ~ rep(Enumerator) ~ WL )
    }
    def For = {
      def Body = rule( '(' ~ Enumerators(false) ~ ')' | '{' ~ Enumerators(true) ~ '}' )
      rule( `for` ~ Body ~ opt(`yield`) ~ Expr0(G) )
    }
    def Throw = rule( `throw` ~ Expr0(G) )
    def Return = rule( `return` ~ opt(Expr0(G)) )
    def Assign = rule( SimpleExpr() ~ `=` ~ Expr0(G) )
    def SmallerExpr = rule( PostfixExpr(G) ~ opt(`match` ~ '{' ~ CaseClauses ~ "}" | Ascription) )
    def LambdaBody = rule( If | While | Try | DoWhile | For | Throw | Return | Assign | SmallerExpr )
    rule( rep(LambdaHead) ~ LambdaBody )
  }

  def PostfixExpr(G: Boolean = false): R0 = {
    def PrefixExpr = rule( opt(WL ~ anyOf("-+~!") ~ WS ~ !Basic.OpChar) ~  SimpleExpr(G) )
    def Check = if (G) OneNLMax else MATCH
    def Check0 = if (G) NotNewline else MATCH
    def InfixExpr = rule( PrefixExpr ~ rep( Check0 ~ Id ~ opt(TypeArgs) ~ Check ~ PrefixExpr) )
    rule( InfixExpr ~ opt(NotNewline ~ Id ~ opt(Newline)) )
  }

  def SimpleExpr(G: Boolean = false): R0 = {
    def Path = rule( rep(Id ~ '.') ~ `this` ~ rep('.' ~ Id) | StableId )
    def Check0 = if (G) NotNewline else MATCH
    def New = rule( `new` ~ NewBody )
    def Parened = rule ( '(' ~ opt(Exprs) ~ ")"  )
    def SimpleExpr1 = rule( XmlExpr | New | BlockExpr | Literal | Path | `_` | Parened)
    rule( SimpleExpr1 ~ rep('.' ~ Id | TypeArgs | Check0 ~ ArgList) ~ opt(Check0  ~ `_`))
  }

  def SimplePat: R0 = {
    def ExtractorArgs = rule( repSep(Pat, ',') )
    def Extractor = rule( StableId ~ opt('(' ~ ExtractorArgs ~ ')') )
    def TupleEx = rule( '(' ~ opt(ExtractorArgs) ~ ')' )
    def Thingy = rule( `_` ~ opt(`:` ~ TypePat) ~ !"*" )
    rule( XmlPattern | Thingy | Literal | TupleEx | Extractor | VarId)
  }

  def BlockExpr: R0 = rule( '{' ~ (CaseClauses | Block) ~ `}` )

  def BlockStats: R0 = {
    def Prelude = rule( rep(Annot) ~ opt(`implicit`) ~ opt(`lazy`) ~ rep(LocalMod) )
    def Tmpl = rule( Prelude ~ BlockDef )
    def BlockStat = rule( Import | Tmpl | Expr0(true) )
    rule( rep1Sep(BlockStat, Semis) )
  }

  def Block: R0 = {
    def End = rule( opt(Semis) ~ &("}" | `case`) )
    def ResultExpr = rule{ Expr0(true) | LambdaHead ~ Block}
    def Body = rule( ResultExpr ~ End | BlockStats ~ opt(Semis ~ ResultExpr) ~ End | End )
    rule( rep(LambdaHead) ~ opt(Semis) ~ Body )
  }

  def Guard(G: Boolean = false): R0 = rule( `if` ~ PostfixExpr(G) )
  def Patterns: R0 = rule( rep1Sep(Pat, ",") )
  def Pat: R0 = rule( rep1Sep(Pat1, '|') )
  def Pat1: R0 = rule( `_` ~ `:` ~ TypePat | VarId ~ `:` ~ TypePat | Pat2 )
  def Pat2: R0 = {
    def Pat3 = rule( `_*` | SimplePat ~ rep(Id ~ SimplePat) )
    rule( VarId ~ `@` ~ Pat3 | Pat3 | VarId )
  }

  def TypePat = rule( CompoundType )


  def ArgList: R0 = rule( '(' ~ opt(Exprs ~ opt(`:` ~ `_*`)) ~ ")" | OneNLMax ~ BlockExpr )

  def CaseClauses: R0 = {
    def CaseClause: R0 = rule( `case` ~ Pat ~ opt(Guard()) ~ `=>` ~ Block )
    rule( rep1(CaseClause) )
  }
}
