package scalaParser
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
    def ImportExpr: R0 = rule( StableId ~ ('.' ~ (`_` | Selectors)).? )
    def Selectors: R0 = rule( '{' ~ (Selector ~ ',').* ~ (Selector | `_`) ~ "}" )
    def Selector: R0 = rule( Id ~ (`=>` ~ (Id | `_`)).? )
    rule( `import` ~ ImportExpr.+.sep(',') )
  }

  def Ascription = rule( `:` ~ (`_*` |  Type | Annot.+) )

  def LambdaHead: R0 = {
    def Binding = rule( (Id | `_`) ~ (`:` ~ Type).? )
    def Bindings = rule( '(' ~ Binding.*.sep(',') ~ ')' )
    def Implicit = rule( `implicit`.? ~ Id ~ (`:` ~ InfixType).? )
    rule( (Bindings | Implicit | `_` ~ Ascription.?) ~ `=>` )
  }

  def Expr = Expr0()

  def Expr0(G: Boolean = false): R0 = {
    def If = {
      def Else = rule( Semi.? ~ `else` ~ Expr0(G) )
      rule( `if` ~ '(' ~ Expr ~ ')' ~ Expr0(G) ~ Else.? )
    }
    def While = rule( `while` ~ '(' ~ Expr ~ ')' ~ Expr0(G) )
    def Try = {
      def Catch = rule( `catch` ~ Expr0(G) )
      def Finally = rule( `finally` ~ Expr0(G) )
      rule( `try` ~ Expr0(G) ~ Catch.? ~ Finally.? )
    }
    def DoWhile = rule( `do` ~ Expr0(G) ~ Semi.? ~ `while` ~ '(' ~ Expr ~ ")" )
    def Enumerators(G: Boolean) = {
      def Generator = rule( Pat1 ~ `<-` ~ Expr0(G) ~ Guard(G).? )
      def Enumerator = rule( Semis ~ Generator | optional(Semis) ~ Guard(G) | Semis ~ Pat1 ~ `=` ~ Expr0(G) )
      rule( Generator ~ Enumerator.* ~ WL )
    }
    def For = {
      def Body = rule( '(' ~ Enumerators(false) ~ ')' | '{' ~ Enumerators(true) ~ '}' )
      rule( `for` ~ Body ~ `yield`.? ~ Expr0(G) )
    }
    def Throw = rule( `throw` ~ Expr0(G) )
    def Return = rule( `return` ~ Expr0(G).? )

    def SmallerExpr = rule( PostfixExpr(G) ~ (`match` ~ '{' ~ CaseClauses ~ "}" | Ascription).? )
    def LambdaBody = rule( If | While | Try | DoWhile | For | Throw | Return | SmallerExpr )
    rule( LambdaHead.* ~ LambdaBody )
  }

  def PostfixExpr(G: Boolean = false): R0 = {
    def Prefixed = rule( (WL ~ anyOf("-+~!") ~ WS ~ !Basic.OpChar) ~  SimpleExpr(G) )
    def Assign = rule( SimpleExpr(G) ~ (`=` ~ Expr0(G)).? )
    def PrefixExpr = rule( Prefixed | Assign )
    def Check = if (G) OneNLMax else MATCH
    def Check0 = if (G) NotNewline else MATCH
    def InfixExpr = rule( PrefixExpr ~ (Check0 ~ Id ~ TypeArgs.? ~ Check ~ PrefixExpr).* )
    rule( InfixExpr ~ (NotNewline ~ Id ~ Newline.?).? )
  }

  def SimpleExpr(G: Boolean = false): R0 = {
    def Path = rule( (Id ~ '.').* ~ `this` ~ ('.' ~ Id).* | StableId )
    def Check0 = if (G) NotNewline else MATCH
    def New = rule( `new` ~ NewBody )
    def Parened = rule ( '(' ~ Exprs.? ~ ")"  )
    def SimpleExpr1 = rule( XmlExpr | New | BlockExpr | Literal | Path | `_` | Parened)
    rule( SimpleExpr1 ~ ('.' ~ Id | TypeArgs | Check0 ~ ArgList).* ~ (Check0  ~ `_`).?)
  }

  def SimplePat: R0 = {
    def ExtractorArgs = rule( Pat.*.sep(',') )
    def Extractor = rule( StableId ~ ('(' ~ ExtractorArgs ~ ')').? )
    def TupleEx = rule( '(' ~ ExtractorArgs.? ~ ')' )
    def Thingy = rule( `_` ~ (`:` ~ TypePat).? ~ !"*" )
    rule( XmlPattern | Thingy | Literal | TupleEx | Extractor | VarId)
  }

  def BlockExpr: R0 = rule( '{' ~ (CaseClauses | Block) ~ `}` )

  def BlockStats: R0 = {
    def Prelude = rule( Annot.* ~ `implicit`.? ~ `lazy`.? ~ LocalMod.* )
    def Tmpl = rule( Prelude ~ BlockDef )
    def BlockStat = rule( Import | Tmpl | Expr0(true) )
    rule( BlockStat.+.sep(Semis) )
  }

  def Block: R0 = {
    def End = rule( Semis.? ~ &("}" | `case`) )
    def ResultExpr = rule{ Expr0(true) | LambdaHead ~ Block}
    def Body = rule( ResultExpr ~ End | BlockStats ~ (Semis ~ ResultExpr).? ~ End | End )
    rule( LambdaHead.* ~ Semis.? ~ Body )
  }

  def Guard(G: Boolean = false): R0 = rule( `if` ~ PostfixExpr(G) )
  def Patterns: R0 = rule( Pat.+.sep(",") )
  def Pat: R0 = rule( Pat1.+.sep('|') )
  def Pat1: R0 = rule( `_` ~ `:` ~ TypePat | VarId ~ `:` ~ TypePat | Pat2 )
  def Pat2: R0 = {
    def Pat3 = rule( `_*` | SimplePat ~ (Id ~ SimplePat).* )
    rule( VarId ~ `@` ~ Pat3 | Pat3 | VarId )
  }

  def TypePat = rule( CompoundType )


  def ArgList: R0 = rule( '(' ~ (Exprs ~ (`:` ~ `_*`).?).? ~ ")" | OneNLMax ~ BlockExpr )

  def CaseClauses: R0 = {
    def CaseClause: R0 = rule( `case` ~ Pat ~ Guard().? ~ `=>` ~ Block )
    rule( CaseClause.+ )
  }
}
