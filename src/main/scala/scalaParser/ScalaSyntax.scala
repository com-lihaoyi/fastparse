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
  extends Core {

  import KeyWordOperators._
  import KeyWordOperators.`_`

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
  def AnnotType = rule(SimpleType ~ opt(NotNewline ~ rep1(NotNewline ~ Annotation)) )

  def SimpleType: R0 = {
    def BasicType = rule( '(' ~ Types ~ ')'  | StableId ~ '.' ~ `type` | StableId )
    rule( BasicType ~ rep(TypeArgs | `#` ~ Id) )
  }

  def TypeArgs = rule( '[' ~ Types ~ "]" )
  def Types = rule( rep1Sep(Type, ',') )
  def TypePat = rule( CompoundType )
  def Ascription = rule( ":" ~ (`_*` |  Type | rep1(Annotation)) )

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

    def For = {
      def Generator = rule( Pattern1 ~ `<-` ~ Expr0(G) ~ opt(Guard(G)) )
      def Enumerator = rule( Generator | Guard(G) | Pattern1 ~ `=` ~ Expr0(G) )
      def Enumerators = rule( Generator ~ rep(Semis ~ Enumerator) ~ WL )
      def Body = rule( '(' ~ Enumerators ~ ')' | '{' ~ Enumerators ~ '}' )
      rule( `for` ~ Body ~ opt(`yield`) ~ Expr0(G) )
    }
    def Throw = rule( `throw` ~ Expr0(G) )
    def Return = rule( `return` ~ opt(Expr0(G)) )
    def Assign = rule( SimpleExpr() ~ `=` ~ Expr0(G) )
    def SmallerExpr = rule( PostfixExpr(G) ~ opt(`match` ~ '{' ~ CaseClauses ~ "}" | Ascription) )
    def Body = rule( If | While | Try | DoWhile | For | Throw | Return | Assign | SmallerExpr )
    rule( rep(LambdaHead) ~ Body )
  }

  def PostfixExpr(G: Boolean = false): R0 = {
    def PrefixExpr = rule( opt(WL ~ anyOf("-+~!") ~ WS ~ !Basic.OperatorChar) ~  SimpleExpr(G) )
    def Check = if (G) OneNLMax else MATCH
    def Check0 = if (G) NotNewline else MATCH
    def InfixExpr = rule( PrefixExpr ~ rep( Check0 ~ Id ~ opt(TypeArgs) ~ Check ~ PrefixExpr) )
    rule( InfixExpr ~ opt(NotNewline ~ Id ~ opt(Newline)) )
  }

  def SimpleExpr(G: Boolean = false): R0 = {
    def Path: R0 = rule( rep(Id ~ '.') ~ `this` ~ rep('.' ~ Id) | StableId )
    def Check0 = if (G) NotNewline else MATCH
    def New = rule( `new` ~ (ClassTemplate | TemplateBody) )
    def SimpleExpr1 = rule( New | BlockExpr | Literal | Path | `_` | '(' ~ opt(Exprs) ~ ")" )
    rule( SimpleExpr1 ~ rep('.' ~ Id | TypeArgs | Check0 ~ ArgumentExprs) ~ opt(Check0  ~ `_`) )
  }

  def Exprs: R0 = rule( rep1Sep(Expr, ',') )
  def ArgumentExprs: R0 = rule(
    '(' ~ opt(Exprs ~ opt(`:` ~ `_*`)) ~ ")" | OneNLMax ~ BlockExpr
  )

  def BlockExpr: R0 = rule( '{' ~ (CaseClauses | Block) ~ `}` )

  def BlockStats: R0 = {
    def Prelude: R0 = rule( rep(Annotation) ~ opt(`implicit`) ~ opt(`lazy`) ~ rep(LocalModifier) )
    def Template: R0 = rule( Prelude ~ (Def | TmplDef) )
    def BlockStat: R0 = rule( Import | Template | Expr0(true) )
    rule( rep1Sep(BlockStat, Semis) )
  }

  def Block: R0 = {
    def End: R0 = rule( opt(Semis) ~ &("}" | `case`) )
    def ResultExpr: R0 = rule{ Expr0(true) | LambdaHead ~ Block}
    def Body = rule( (ResultExpr ~ End | BlockStats ~ opt(Semis ~ ResultExpr) ~ End | End) )
    rule( rep(LambdaHead) ~ opt(Semis) ~ Body )
  }

  def CaseClauses: R0 = {
    def CaseClause: R0 = rule( `case` ~ Pattern ~ opt(Guard()) ~ `=>` ~ Block )
    rule( rep1(CaseClause) )
  }

  def Guard(G: Boolean = false): R0 = rule( `if` ~ PostfixExpr(G) )
  def Pattern: R0 = rule( rep1Sep(Pattern1, '|') )
  def Pattern1: R0 = rule( `_` ~ `:` ~ TypePat | VarId ~ `:` ~ TypePat | Pattern2 )
  def Pattern2: R0 = {
    def Pattern3: R0 = rule( `_*` | SimplePattern ~ rep(Id ~ SimplePattern) )
    rule( VarId ~ `@` ~ Pattern3 | Pattern3 | VarId )
  }

  def SimplePattern: R0 = {
    def ExtractorArgs = rule( repSep(Pattern, ',') )
    def Extractor = rule( StableId ~ opt('(' ~ ExtractorArgs ~ ')') )
    def TupleEx = rule( '(' ~ opt(ExtractorArgs) ~ ')' )
    def Thingy = rule( `_` ~ opt(`:` ~ TypePat) ~ !"*" )
    rule( Thingy | Literal | TupleEx | Extractor | VarId )
  }

  def TypeParamClause: R0 = {
    def VariantParam: R0 = rule( rep(Annotation) ~ opt(WL ~ anyOf("+-")) ~ TypeParam )
    rule( '[' ~ rep1Sep(VariantParam, ',') ~ ']' )
  }

  def TypeBounds: R0 = rule( opt(`>:` ~ Type) ~ opt(`<:` ~ Type) )
  def TypeParam: R0 = rule (
    (Id | `_`) ~ opt(TypeParamClause) ~ TypeBounds ~ rep(`<%` ~ Type) ~ rep(`:` ~ Type)
  )

  def Modifier: R0 = rule( LocalModifier | AccessModifier | `override` )
  def LocalModifier: R0 = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  def AccessModifier: R0 = {
    def AccessQualifier: R0 = rule( '[' ~ (`this` | Id) ~ ']' )
    rule( (`private` | `protected`) ~ opt(AccessQualifier) )
  }

  def Annotation: R0 = rule( `@` ~ SimpleType ~  rep(ArgumentExprs)  )

  def TemplateBody: R0 = {
    def Prelude = rule( rep(Annotation ~ OneNLMax) ~ rep(Modifier) )
    def TemplateStat = rule( Import | Prelude ~ (Def | Dcl) | Expr0(true) )
    def ThisType = rule( `this` ~ `:` ~ InfixType ~ `=>` )
    def OtherType = rule( (Id | `_`) ~ opt(`:` ~ InfixType) ~ `=>` )
    def SelfType: R0 = rule( ThisType | OtherType)
    rule( '{' ~ opt(SelfType) ~ opt(Semis) ~ repSep(TemplateStat, Semis) ~ `}` )
  }

  def Import: R0 = {
    def ImportExpr: R0 = rule( StableId ~ opt('.' ~ (`_` | Selectors)) )
    def Selectors: R0 = rule( '{' ~ rep(Selector ~ ',') ~ (Selector | `_`) ~ "}" )
    def Selector: R0 = rule( Id ~ opt(`=>` ~ (Id | `_`)) )
    rule( `import` ~ rep1Sep(ImportExpr, ',') )
  }

  def Dcl: R0 = {
    def VarDcl: R0 = rule( `var` ~ Ids ~ `:` ~ Type )
    def FunDcl: R0 = rule( `def` ~ FunSig ~ opt(`:` ~ Type) )
    rule( ValDcl | VarDcl | FunDcl | TypeDcl )
  }
  def FunSig: R0 = {
    def FunTypeParamClause: R0 = rule( '[' ~ rep1Sep(rep(Annotation) ~ TypeParam, ',') ~ ']' )
    def ParamClauses: R0 = rule( rep(ParamClause) ~ opt(OneNLMax ~ '(' ~ `implicit` ~ Params ~ ')') )
    def ParamClause: R0 = rule( OneNLMax ~ '(' ~ opt(Params) ~ ')' )
    def Param: R0 = rule( rep(Annotation) ~ Id ~ opt(`:` ~ ParamType) ~ opt(`=` ~ Expr) )
    def Params: R0 = rule( rep1Sep(Param, ',') )
    rule( (Id | `this`) ~ opt(FunTypeParamClause) ~ ParamClauses )
  }
  def ValDcl: R0 = rule( `val` ~ Ids ~ `:` ~ Type )
  def TypeDcl: R0 = rule( `type` ~ Id ~ opt(TypeParamClause) ~ TypeBounds )

  def ValVarDef: R0 = {
    def ValDef: R0 = rule( rep1Sep(Pattern2, ',') ~ opt(`:` ~ Type) ~ `=` ~ Expr0(true) )
    def VarDef: R0 = rule( Ids ~ `:` ~ Type ~ `=` ~ `_` | ValDef )
    rule( `val` ~ ValDef | `var` ~ VarDef )
  }
  def Def: R0 = {
    def Body: R0 = rule( `=` ~ opt(`macro`) ~ Expr0(true) | OneNLMax ~ '{' ~ Block ~ "}" )
    def FunDef: R0 = rule( `def` ~ FunSig ~ opt(`:` ~ Type) ~ Body )
    rule( FunDef | TypeDef | ValVarDef | TmplDef )
  }

  def TypeDef: R0 = rule( `type` ~ Id ~ opt(TypeParamClause) ~ `=` ~ Type )

  def TmplDef: R0 = {
    def ClassDef: R0 = {
      def Annot = rule( `@` ~ SimpleType ~ ArgumentExprs )
      def ConstrPrelude = {
        rule( NotNewline ~ ( rep1(Annot) ~ opt(AccessModifier) | rep(Annot) ~ AccessModifier) )
      }
      def Mod = rule( opt(rep(Modifier) ~ (`val` | `var`)) )
      def ClassParam = rule( rep(Annotation) ~ Mod ~ Id ~ `:` ~ ParamType ~ opt(`=` ~ Expr) )

      def Implicit = rule( OneNLMax ~ '(' ~ `implicit` ~ rep1Sep(ClassParam, ",") ~ ")" )
      def ClassParams = rule( rep1Sep(ClassParam, ',') )
      def ClassParamClause = rule( OneNLMax ~'(' ~ opt(ClassParams) ~ ")" )
      def ClassParamClauses = rule( rep1(ClassParamClause) ~ opt(Implicit) | Implicit )
      rule {
        `class` ~ Id ~
        opt(TypeParamClause) ~
        opt(ConstrPrelude) ~
        opt(ClassParamClauses) ~
        ClassTemplateOpt
      }
    }
    def TraitTemplateOpt: R0 = {
      def TraitParents: R0 = rule( AnnotType ~ rep(`with` ~ AnnotType) )
      def TraitTemplate: R0 = rule( opt(EarlyDefs) ~ TraitParents ~ opt(TemplateBody) )
      rule( `extends` ~ TraitTemplate | opt(opt(`extends`) ~ TemplateBody) )
    }
    def TraitDef: R0 = rule( `trait` ~ Id ~ opt(TypeParamClause) ~ TraitTemplateOpt )
    rule( TraitDef | opt(`case`) ~ (ClassDef | ObjectDef) )
  }

  def ObjectDef: R0 = rule( `object` ~ Id ~ ClassTemplateOpt )
  def ClassTemplateOpt: R0 = rule {
    `extends` ~ ClassTemplate | opt(opt(`extends`) ~ TemplateBody)
  }

  def ClassTemplate: R0 = {
    def Constr: R0 = rule( AnnotType ~ rep(NotNewline ~ ArgumentExprs) )
    def ClassParents: R0 = rule( Constr ~ rep(`with` ~ AnnotType) )
    rule( opt(EarlyDefs) ~ ClassParents ~ opt(TemplateBody) )
  }

  def EarlyDefs: R0 = {
    def EarlyDef: R0 = rule( rep(Annotation ~ OneNLMax) ~ rep(Modifier) ~ ValVarDef )
    rule( `{` ~ repSep(EarlyDef, Semis) ~ `}` ~ `with` )
  }

  def TopStatSeq: R0 = {
    def PackageObject: R0 = rule( `package` ~ ObjectDef )
    def Packaging: R0 = rule( `package` ~ QualId ~ `{` ~ opt(TopStatSeq) ~ `}` )
    def Template = rule( rep(Annotation ~ OneNLMax) ~ rep(Modifier) ~ TmplDef )
    def TopStat: R0 = rule( Packaging | PackageObject | Import | Template )
    rule( rep1Sep(TopStat, Semis) )
  }

  def CompilationUnit: Rule1[String] = {
    def TopPackageSeq = rule( rep1Sep(`package` ~ QualId ~ !(WS ~ "{"), Semis) )
    def Body = rule( TopPackageSeq ~ opt(Semis ~ TopStatSeq) | TopStatSeq | MATCH )
    rule( capture(opt(Semis) ~ Body ~ opt(Semis) ~ WL) )
  }
}
