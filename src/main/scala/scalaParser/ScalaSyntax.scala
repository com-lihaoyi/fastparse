package scalaParser
import acyclic.file
import language.implicitConversions
import syntax._
import org.parboiled2._
import macros.Macros._
/**
 * Parser for Scala syntax.
 */
class ScalaSyntax (val input: ParserInput)
  extends Core with Xml{

  import KeyWordOperators._
  import KeyWordOperators.`_`

  private[this] implicit def wspStr(s: String): R0 = rule( WL ~ str(s) )

  private[this] implicit def wspCh(s: Char): R0 = rule( WL ~ ch(s) )

  def Type: R0 = {
    def FunctionArgTypes = rule('(' ~ opt(rep1Sep(ParamType, ',')) ~ ')' )
    def ArrowType = rule( FunctionArgTypes ~ `=>` ~ Type )
    def ExistentialClause = rule( `forSome` ~ `{` ~ rep1Sep(TypeDcl | ValDcl, Semis) ~ `}` )
    def PostfixType = rule( InfixType ~ (`=>` ~ Type | opt(ExistentialClause)) )
    def Unbounded = rule( `_` | ArrowType | PostfixType )
    rule( Unbounded ~ TypeBounds )
  }

  def InfixType = rule( CompoundType ~ rep(NotNewline ~ Id ~ OneNLMax ~ CompoundType) )

  def CompoundType = {
    def RefineStat = rule( TypeDef | Dcl  )
    def Refinement = rule( OneNLMax ~ `{` ~ repSep(RefineStat, Semis) ~ `}` )
    rule( rep1Sep(AnnotType, `with`) ~ opt(Refinement) | Refinement )
  }
  def AnnotType = rule(SimpleType ~ opt(NotNewline ~ rep1(NotNewline ~ Annot)) )

  def SimpleType: R0 = {
    def BasicType = rule( '(' ~ Types ~ ')'  | StableId ~ '.' ~ `type` | StableId )
    rule( BasicType ~ rep(TypeArgs | `#` ~ Id) )
  }

  def TypeArgs = rule( '[' ~ Types ~ "]" )
  def Types = rule( rep1Sep(Type, ',') )
  def TypePat = rule( CompoundType )
  def Ascription = rule( `:` ~ (`_*` |  Type | rep1(Annot)) )

  def ParamType = rule( `=>` ~ Type | Type ~ "*" | Type )

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
    def New = rule( `new` ~ (ClsTmpl | TmplBody) )
    def Parened = rule ( '(' ~ opt(Exprs) ~ ")"  )
    def SimpleExpr1 = rule( Xml.XmlExpr | New | BlockExpr | Literal | Path | `_` | Parened)
    rule( SimpleExpr1 ~ rep('.' ~ Id | TypeArgs | Check0 ~ ArgList) ~ opt(Check0  ~ `_`))
  }

  def Exprs: R0 = rule( rep1Sep(Expr, ',') )
  def ArgList: R0 = rule( '(' ~ opt(Exprs ~ opt(`:` ~ `_*`)) ~ ")" | OneNLMax ~ BlockExpr )

  def BlockExpr: R0 = rule( '{' ~ (CaseClauses | Block) ~ `}` )

  def BlockStats: R0 = {
    def Prelude = rule( rep(Annot) ~ opt(`implicit`) ~ opt(`lazy`) ~ rep(LocalMod) )
    def Template = rule( Prelude ~ (Def | TmplDef) )
    def BlockStat = rule( Import | Template | Expr0(true) )
    rule( rep1Sep(BlockStat, Semis) )
  }

  def Block: R0 = {
    def End = rule( opt(Semis) ~ &("}" | `case`) )
    def ResultExpr = rule{ Expr0(true) | LambdaHead ~ Block}
    def Body = rule( ResultExpr ~ End | BlockStats ~ opt(Semis ~ ResultExpr) ~ End | End )
    rule( rep(LambdaHead) ~ opt(Semis) ~ Body )
  }

  def CaseClauses: R0 = {
    def CaseClause: R0 = rule( `case` ~ Pat ~ opt(Guard()) ~ `=>` ~ Block )
    rule( rep1(CaseClause) )
  }

  def Guard(G: Boolean = false): R0 = rule( `if` ~ PostfixExpr(G) )
  def Patterns: R0 = rule( rep1Sep(Pat, ",") )
  def Pat: R0 = rule( rep1Sep(Pat1, '|') )
  def Pat1: R0 = rule( `_` ~ `:` ~ TypePat | VarId ~ `:` ~ TypePat | Pat2 )
  def Pat2: R0 = {
    def Pat3 = rule( `_*` | SimplePat ~ rep(Id ~ SimplePat) )
    rule( VarId ~ `@` ~ Pat3 | Pat3 | VarId )
  }

  def SimplePat: R0 = {
    def ExtractorArgs = rule( repSep(Pat, ',') )
    def Extractor = rule( StableId ~ opt('(' ~ ExtractorArgs ~ ')') )
    def TupleEx = rule( '(' ~ opt(ExtractorArgs) ~ ')' )
    def Thingy = rule( `_` ~ opt(`:` ~ TypePat) ~ !"*" )
    rule( Xml.XmlPattern | Thingy | Literal | TupleEx | Extractor | VarId)
  }

  def TypeArgList: R0 = {
    def Variant: R0 = rule( rep(Annot) ~ opt(WL ~ anyOf("+-")) ~ TypeArg )
    rule( '[' ~ rep1Sep(Variant, ',') ~ ']' )
  }

  def TypeBounds: R0 = rule( opt(`>:` ~ Type) ~ opt(`<:` ~ Type) )
  def TypeArg: R0 = {
    def CtxBounds = rule(rep(`<%` ~ Type) ~ rep(`:` ~ Type))
    rule((Id | `_`) ~ opt(TypeArgList) ~ TypeBounds ~ CtxBounds)
  }

  def Annot: R0 = rule( `@` ~ SimpleType ~  rep(ArgList)  )

  def TmplBody: R0 = {
    def Prelude = rule( rep(Annot ~ OneNLMax) ~ rep(Mod) )
    def TmplStat = rule( Import | Prelude ~ (Def | Dcl) | Expr0(true) )
    def ThisType = rule( `this` ~ `:` ~ InfixType ~ `=>` )
    def OtherType = rule( (Id | `_`) ~ opt(`:` ~ InfixType) ~ `=>` )
    def SelfType = rule( ThisType | OtherType)
    rule( '{' ~ opt(SelfType) ~ opt(Semis) ~ repSep(TmplStat, Semis) ~ `}` )
  }

  def Import: R0 = {
    def ImportExpr: R0 = rule( StableId ~ opt('.' ~ (`_` | Selectors)) )
    def Selectors: R0 = rule( '{' ~ rep(Selector ~ ',') ~ (Selector | `_`) ~ "}" )
    def Selector: R0 = rule( Id ~ opt(`=>` ~ (Id | `_`)) )
    rule( `import` ~ rep1Sep(ImportExpr, ',') )
  }

  def Dcl: R0 = {
    def VarDcl = rule( `var` ~ Ids ~ `:` ~ Type )
    def FunDcl = rule( `def` ~ FunSig ~ opt(`:` ~ Type) )
    rule( ValDcl | VarDcl | FunDcl | TypeDcl )
  }
  def FunSig: R0 = {
    def FunTypeArgs = rule( '[' ~ rep1Sep(rep(Annot) ~ TypeArg, ',') ~ ']' )
    def FunAllArgs = rule( rep(FunArgs) ~ opt(OneNLMax ~ '(' ~ `implicit` ~ Args ~ ')') )
    def FunArgs = rule( OneNLMax ~ '(' ~ opt(Args) ~ ')' )
    def FunArg = rule( rep(Annot) ~ Id ~ opt(`:` ~ ParamType) ~ opt(`=` ~ Expr) )
    def Args = rule( rep1Sep(FunArg, ',') )
    rule( (Id | `this`) ~ opt(FunTypeArgs) ~ FunAllArgs )
  }
  def ValDcl: R0 = rule( `val` ~ Ids ~ `:` ~ Type )
  def TypeDcl: R0 = rule( `type` ~ Id ~ opt(TypeArgList) ~ TypeBounds )

  def ValVarDef: R0 = {
    def Val = rule( rep1Sep(Pat2, ',') ~ opt(`:` ~ Type) ~ `=` ~ Expr0(true) )
    def Var = rule( Ids ~ `:` ~ Type ~ `=` ~ `_` | Val )
    rule( `val` ~ Val | `var` ~ Var )
  }
  def Def: R0 = {
    def Body = rule( `=` ~ opt(`macro`) ~ Expr0(true) | OneNLMax ~ '{' ~ Block ~ "}" )
    def FunDef = rule( `def` ~ FunSig ~ opt(`:` ~ Type) ~ Body )
    rule( FunDef | TypeDef | ValVarDef | TmplDef )
  }

  def TypeDef: R0 = rule( `type` ~ Id ~ opt(TypeArgList) ~ `=` ~ Type )

  def TmplDef: R0 = {
    def ClsDef = {
      def ClsAnnot = rule( `@` ~ SimpleType ~ ArgList )
      def Prelude = rule( NotNewline ~ ( rep1(ClsAnnot) ~ opt(AccessMod) | rep(ClsAnnot) ~ AccessMod) )
      def ClsArgMod = rule( opt(rep(Mod) ~ (`val` | `var`)) )
      def ClsArg = rule( rep(Annot) ~ ClsArgMod ~ Id ~ `:` ~ ParamType ~ opt(`=` ~ Expr) )

      def Implicit = rule( OneNLMax ~ '(' ~ `implicit` ~ rep1Sep(ClsArg, ",") ~ ")" )
      def ClsArgs = rule( OneNLMax ~'(' ~ repSep(ClsArg, ',') ~ ")" )
      def AllArgs = rule( rep1(ClsArgs) ~ opt(Implicit) | Implicit )
      rule( `class` ~ Id ~ opt(TypeArgList) ~ opt(Prelude) ~ opt(AllArgs) ~ ClsTmplOpt )
    }
    def TraitTmplOpt = {
      def TraitParents = rule( AnnotType ~ rep(`with` ~ AnnotType) )
      def TraitTmpl = rule( opt(EarlyDefs) ~ TraitParents ~ opt(TmplBody) )
      rule( `extends` ~ TraitTmpl | opt(opt(`extends`) ~ TmplBody) )
    }
    def TraitDef = rule( `trait` ~ Id ~ opt(TypeArgList) ~ TraitTmplOpt )
    rule( TraitDef | opt(`case`) ~ (ClsDef | ObjDef) )
  }

  def ObjDef: R0 = rule( `object` ~ Id ~ ClsTmplOpt )
  def ClsTmplOpt: R0 = rule( `extends` ~ ClsTmpl | opt(opt(`extends`) ~ TmplBody) )

  def ClsTmpl: R0 = {
    def Constr = rule( AnnotType ~ rep(NotNewline ~ ArgList) )
    def ClsParents = rule( Constr ~ rep(`with` ~ AnnotType) )
    rule( opt(EarlyDefs) ~ ClsParents ~ opt(TmplBody) )
  }

  def EarlyDefs: R0 = {
    def EarlyDef = rule( rep(Annot ~ OneNLMax) ~ rep(Mod) ~ ValVarDef )
    rule( `{` ~ repSep(EarlyDef, Semis) ~ `}` ~ `with` )
  }

  def TopStatSeq: R0 = {
    def PkgObj = rule( `package` ~ ObjDef )
    def PkgBlock = rule( `package` ~ QualId ~ `{` ~ opt(TopStatSeq) ~ `}` )
    def Tmpl = rule( rep(Annot ~ OneNLMax) ~ rep(Mod) ~ TmplDef )
    def TopStat = rule( PkgBlock | PkgObj | Import | Tmpl )
    rule( rep1Sep(TopStat, Semis) )
  }

  def CompilationUnit: Rule1[String] = {
    def TopPackageSeq = rule( rep1Sep(`package` ~ QualId ~ !(WS ~ "{"), Semis) )
    def Body = rule( TopPackageSeq ~ opt(Semis ~ TopStatSeq) | TopStatSeq | MATCH )
    rule( capture(opt(Semis) ~ Body ~ opt(Semis) ~ WL) )
  }
}
