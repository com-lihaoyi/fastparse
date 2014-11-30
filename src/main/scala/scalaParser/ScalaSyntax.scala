package scalaParser
import acyclic.file
import language.implicitConversions
import syntax._
import org.parboiled2._
import macros.Macros._
/**
 * Parser for Scala syntax.
 */
class ScalaSyntax (val input: ParserInput) extends Parser with Basic with Identifiers with Literals {
  // Aliases for common things. These things are used in almost every parser
  // in the file, so it makes sense to keep them short.
  type R0 = Rule0
  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  def WS = rule( rep(Basic.WhitespaceChar | Literals.Comment) )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL = rule( rep(Basic.WhitespaceChar | Literals.Comment | Basic.Newline) )


  /**
   * By default, all strings and characters greedily
   * capture all whitespace immediately before the token.
   */
  implicit private[this] def wspStr(s: String): R0 = rule( WL ~ str(s)  )
  implicit private[this] def wspChar(s: Char): R0 = rule( WL ~ ch(s) )

  /**
   * Most keywords don't just require the correct characters to match,
   * they have to ensure that subsequent characters *don't* match in
   * order for it to be a keyword. This enforces that rule for key-words
   * (W) and key-operators (O) which have different non-match criteria.
   */
  object KeyWordOperators {
    private[this] def W(s: String) = rule( WL ~ Key.W(s) )
    private[this] def O(s: String) = rule( WL ~ Key.O(s) )

    def `=>` = rule( O("=>") | O("⇒") )
    def `<-` = rule( O("<-") | O("←") )
    def `:` = O(":")
    def `=` = O("=")
    def `@` = O("@")
    def `_` = W("_")
    def `this` = W("this")
    def `type` = W("type")
    def `val` = W("val")
    def `var` = W("var")
    def `def` = W("def")
    def `with` = W("with")
    def `package` = W("package")
    def `object` = W("object")
    def `class` = W("class")
    def `case` = W("case")
    def `trait` = W("trait")
    def `extends` = W("extends")
    def `implicit` = W("implicit")
    def `try` = W("try")
    def `new` = W("new")
    def `macro` = W("macro")
    def `import` = W("import")
    def `else` = W("else")
    def `super` = W("super")
    def `catch` = W("catch")
    def `finally` = W("finally")
    def `do` = W("do")
    def `yield` = W("yield")
    def `while` = W("while")
    def `<%` = O("<%")
    def `override` = W("override")
    def `#` = O("#")
    def `forSome` = W("forSome")
    def `for` = W("for")
    def `abstract` = W("abstract")
    def `throw` = W("throw")
    def `return` = W("return")
    def `lazy` = W("lazy")
    def `if` = W("if")
    def `match` = W("match")
    def `>:` = O(">:")
    def `<:` = O("<:")
    def `final` =  W("final")
    def `sealed` = W("sealed")
    def `private` = W("private")
    def `protected` = W("protected")
  }
  import KeyWordOperators._
  import KeyWordOperators.`_`

  def `_*` = rule( `_` ~ "*" )
  def `}` = rule( opt(Semis) ~ '}' )
  def `{` = rule( '{' ~ opt(Semis) )
  /**
   * helper printing function
   */
  def pr(s: String) = rule( run(println(s"LOGGING $cursor: $s")) )

  def Id = rule( WL ~ Identifiers.Id )
  def VarId = rule( WL ~ Identifiers.VarId )
  def Literal = rule( WL ~ Literals.Literal )
  def Semi = rule( WS ~ Basic.Semi )
  def Semis = rule( rep1(Semi) )
  def Newline = rule( WL ~ Basic.Newline )

  def QualId = rule( WL ~ rep1Sep(Id, '.') )
  def Ids = rule( rep1Sep(Id, ',') )

  def NotNewline: R0 = rule( &( WS ~ !Basic.Newline ) )
  def OneNLMax: R0 = {
    def ConsumeComments = rule(rep(
      rep(Basic.WhitespaceChar) ~
      Literals.Comment ~
      rep(Basic.WhitespaceChar) ~
      Basic.Newline
    ))
    rule( WS ~ opt(Basic.Newline) ~ ConsumeComments ~ NotNewline )
  }
  def StableId: R0 = {
    def ClassQualifier = rule( '[' ~ Id ~ ']' )
    def ThisSuper = rule( `this` | `super` ~ opt(ClassQualifier) )
    rule( rep(Id ~ '.') ~ ThisSuper ~ rep('.' ~ Id) | Id ~ rep('.' ~ Id) )
  }


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
    def BasicType: R0 = rule( '(' ~ Types ~ ')'  | StableId ~ '.' ~ `type` | StableId )
    rule( BasicType ~ rep(TypeArgs | `#` ~ Id) )
  }

  def TypeArgs = rule( '[' ~ Types ~ "]" )
  def Types = rule( rep1Sep(Type, ',') )
  def TypePat = rule( CompoundType )
  def Ascription = rule( ":" ~ (`_*` |  Type | rep1(Annotation)) )

  def ParamType = rule( `=>` ~ Type | Type ~ "*" | Type )

  def LambdaHead: R0 = {
    def Binding: R0 = rule( (Id | `_`) ~ opt(`:` ~ Type) )
    def Bindings: R0 = rule( '(' ~ repSep(Binding, ',') ~ ')' )
    def Implicit: R0 = rule( opt(`implicit`) ~ Id ~ opt(`:` ~ InfixType) )
    rule( (Bindings | Implicit | `_` ~ opt(Ascription)) ~ `=>` )
  }

  def Expr = Expr0()
  def ExprSensitive = Expr0(true)
  def Expr0(G: Boolean = false): R0 = {
    def IfCFlow = {
      def Else = rule( opt(Semi) ~ `else` ~ Expr0(G) )
      rule( `if` ~ '(' ~ Expr ~ ')' ~ Expr0(G) ~ opt(Else) )
    }
    def WhileCFlow = rule( `while` ~ '(' ~ Expr ~ ')' ~ Expr0(G) )
    def TryCFlow = {
      def Catch = rule( `catch` ~ Expr0(G) )
      def Finally = rule( `finally` ~ Expr0(G) )
      rule( `try` ~ Expr0(G) ~ opt(Catch) ~ opt(Finally) )
    }
    def DoWhileCFlow = rule( `do` ~ Expr0(G) ~ opt(Semi) ~ `while` ~ '(' ~ Expr ~ ")" )
    def Enumerators(G: Boolean = false): R0 = {
      def Generator: R0 = rule( Pattern1 ~ `<-` ~ Expr0(G) ~ opt(Guard(G)) )
      def Enumerator: R0 = rule( Generator | Guard(G) | Pattern1 ~ `=` ~ Expr0(G) )
      rule( Generator ~ rep(Semis ~ Enumerator) ~ WL )
    }
    def ForCFlow = {
      rule {
        `for` ~
        ('(' ~ Enumerators() ~ ')' | '{' ~ Enumerators(true) ~ '}') ~
        opt(`yield`) ~
        Expr0(G)
      }
    }
    rule {
      rep(LambdaHead) ~ (
        IfCFlow |
        WhileCFlow |
        TryCFlow |
        DoWhileCFlow |
        ForCFlow |
        `throw` ~ Expr0(G) |
        `return` ~ opt(Expr0(G)) |
        SimpleExpr() ~ `=` ~ Expr0(G) |
        PostfixExpr(G) ~ opt(`match` ~ '{' ~ CaseClauses ~ "}" | Ascription)
      )
    }
  }

  def PostfixExpr(G: Boolean = false): R0 = {
    def PrefixExpr = rule {
      opt(WL ~ anyOf("-+~!") ~ WS ~ !(Basic.OperatorChar)) ~  SimpleExpr(G)
    }
    def Check = if (G) OneNLMax else MATCH
    def Check0 = if (G) NotNewline else MATCH
    def InfixExpr: R0 = rule {
      PrefixExpr ~ rep( Check0 ~ Id ~ opt(TypeArgs) ~ Check ~ PrefixExpr )
    }
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
    def BlockEnd: R0 = rule( opt(Semis) ~ &("}" | `case`) )
    def ResultExpr: R0 = rule{ Expr0(true) | LambdaHead ~ Block}
    rule {
      rep(LambdaHead) ~
      opt(Semis) ~
      (
        ResultExpr ~ BlockEnd |
        BlockStats ~ opt(Semis ~ ResultExpr) ~ BlockEnd |
        MATCH ~ BlockEnd
      )
    }
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
    def Extractor: R0 = rule( StableId ~ opt('(' ~ ExtractorArgs ~ ')') )
    rule {
      `_` ~ opt(`:` ~ TypePat) ~ !"*" |
      Literal |
      '(' ~ opt(ExtractorArgs) ~ ')' |
      Extractor |
      VarId
    }
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
    def TemplateStat: R0 = rule(
      Import | rep(Annotation ~ OneNLMax) ~ rep(Modifier) ~ (Def | Dcl) | Expr0(true)
    )
    def SelfType: R0 = rule {
      `this` ~ `:` ~ InfixType ~ `=>` | (Id | `_`) ~ opt(`:` ~ InfixType) ~ `=>`
    }

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
    def Params: R0 = rule( repSep(Param, ',') )

    rule( (Id | `this`) ~ opt(FunTypeParamClause) ~ ParamClauses )
  }
  def ValDcl: R0 = rule( `val` ~ Ids ~ `:` ~ Type )
  def TypeDcl: R0 = rule( `type` ~ Id ~ opt(TypeParamClause) ~ TypeBounds )

  def PatVarDef: R0 = {
    def PatDef: R0 = rule( rep1Sep(Pattern2, ',') ~ opt(`:` ~ Type) ~ `=` ~ Expr0(true) )
    def VarDef: R0 = rule( Ids ~ `:` ~ Type ~ `=` ~ `_` | PatDef )
    rule( `val` ~ PatDef | `var` ~ VarDef )
  }
  def Def: R0 = {
    def Body: R0 = rule( `=` ~ opt(`macro`) ~ Expr0(true) | OneNLMax ~ '{' ~ Block ~ "}" )
    def FunDef: R0 = rule( `def` ~ FunSig ~ opt(`:` ~ Type) ~ Body )
    rule( FunDef | TypeDef | PatVarDef | TmplDef )
  }

  def TypeDef: R0 = rule( `type` ~ Id ~ opt(TypeParamClause) ~ `=` ~ Type )

  def TmplDef: R0 = {
    def ClassDef: R0 = {
      def Annot: R0 = rule( `@` ~ SimpleType ~ ArgumentExprs )
      def ConstrPrelude: R0 = {
        rule( NotNewline ~ ( rep1(Annot) ~ opt(AccessModifier) | rep(Annot) ~ AccessModifier) )
      }
      def ClassParam: R0 = rule {
        rep(Annotation) ~
          opt(rep(Modifier) ~ (`val` | `var`)) ~
          Id ~
          `:` ~
          ParamType ~
          opt(`=` ~ Expr)
      }
      def Implicit: R0 = rule( OneNLMax ~ '(' ~ `implicit` ~ rep1Sep(ClassParam, ",") ~ ")" )
      def ClassParams: R0 = rule( rep1Sep(ClassParam, ',') )
      def ClassParamClause: R0 = rule( OneNLMax ~'(' ~ opt(ClassParams) ~ ")" )
      def ClassParamClauses: R0 = rule( rep1(ClassParamClause) ~ opt(Implicit) | Implicit )
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
    def EarlyDef: R0 = rule( rep(Annotation ~ OneNLMax) ~ rep(Modifier) ~ PatVarDef )
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
    def TopPackageSeq: R0 = rule( rep1Sep(`package` ~ QualId ~ !(WS ~ "{"), Semis) )

    rule {
      capture(
        opt(Semis) ~
        (TopPackageSeq ~ opt(Semis ~ TopStatSeq) | TopStatSeq | MATCH) ~
        opt(Semis) ~
        WL
      )
    }
  }

}
