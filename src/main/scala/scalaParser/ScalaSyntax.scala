package scalaParser
import acyclic.file
import language.implicitConversions
import syntax._
import org.parboiled2._

/**
 * Parser for Scala syntax.
 */
class ScalaSyntax(val input: ParserInput) extends Parser with Basic with Identifiers with Literals {
  // Aliases for common things. These things are used in almost every parser
  // in the file, so it makes sense to keep them short.
  type R0 = Rule0
  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  def WS = rule( zeroOrMore(Basic.WhitespaceChar | Literals.Comment) )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL = rule( zeroOrMore(Basic.WhitespaceChar | Literals.Comment | Basic.Newline) )


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
  /**
   * helper printing function
   */
  def pr(s: String) = rule( run(println(s"LOGGING $cursor: $s")) )

  def Id = rule( WL ~ Identifiers.Id )
  def VarId = rule( WL ~ Identifiers.VarId )
  def Literal = rule( WL ~ Literals.Literal )
  def Semi = rule( WS ~ Basic.Semi )
  def Semis = rule( oneOrMore(Semi) )
  def Newline = rule( WL ~ Basic.Newline )

  def QualId = rule( WL ~ oneOrMore(Id).separatedBy('.') )
  def Ids = rule( oneOrMore(Id) separatedBy ',' )

  def NotNewline: R0 = rule( &( WS ~ !Basic.Newline ) )
  def OneNewlineMax: R0 = rule{
    WS ~
    optional(Basic.Newline) ~
    zeroOrMore(
      zeroOrMore(Basic.WhitespaceChar) ~
      Literals.Comment ~
      zeroOrMore(Basic.WhitespaceChar) ~
      Basic.Newline
    ) ~
    NotNewline
  }
  def StableId: R0 = {
    def ClassQualifier = rule( '[' ~ Id ~ ']' )
    rule {
      zeroOrMore(Id ~ '.') ~ (`this` | `super` ~ optional(ClassQualifier)) ~ zeroOrMore('.' ~ Id) |
      Id ~ zeroOrMore('.' ~ Id)
    }
  }
  def ExistentialDcl = rule( `type` ~ TypeDcl | `val` ~ ValDcl )
  def ExistentialClause = rule {
    `forSome` ~ '{' ~ oneOrMore(ExistentialDcl).separatedBy(Semi) ~ '}'
  }
  def Type: R0 = {
    def FunctionArgTypes = rule('(' ~ optional(oneOrMore(ParamType) separatedBy ',') ~ ')' )
    def ArrowType = rule( FunctionArgTypes ~ `=>` ~ Type )
    def PostfixType = rule( InfixType ~ (`=>` ~ Type | optional(ExistentialClause)) )
    def Unbounded = rule( `_` | ArrowType | PostfixType )

    rule( Unbounded ~ TypeBounds )
  }

  def InfixType = rule {
    CompoundType ~ zeroOrMore(NotNewline ~ Id ~ OneNewlineMax ~ CompoundType)
  }
  def CompoundType = {
    def RefineStat = rule( `type` ~ TypeDef | Dcl | MATCH )
    def Refinement = rule {
      OneNewlineMax ~
      '{'  ~
      optional(Semis) ~
      zeroOrMore(RefineStat).separatedBy(Semis) ~
      optional(Semis) ~
      "}"
    }
    rule {
      oneOrMore(AnnotType).separatedBy(`with`) ~ optional(Refinement) |
      Refinement
    }
  }
  def AnnotType = rule(SimpleType ~ optional(NotNewline ~ oneOrMore(NotNewline ~ Annotation)) )

  def SimpleType: R0 = {
    def BasicType: R0 = rule( '(' ~ Types ~ ')'  | StableId ~ '.' ~ `type` | StableId )
    rule( BasicType ~ zeroOrMore(TypeArgs | `#` ~ Id) )
  }

  def TypeArgs = rule( '[' ~ Types ~ "]" )
  def Types = rule( oneOrMore(Type).separatedBy(',') )


  def TypePat = rule( CompoundType )

  def Ascription = rule( ":" ~ (`_*` |  Type | oneOrMore(Annotation)) )

  def ParamType = rule( `=>` ~ Type | Type ~ "*" | Type )

  def LambdaHead: R0 = {
    def Binding: R0 = rule( (Id | `_`) ~ optional(`:` ~ Type) )
    def Bindings: R0 = rule( '(' ~ zeroOrMore(Binding).separatedBy(',') ~ ')' )
    def Implicit: R0 = rule( optional(`implicit`) ~ Id ~ optional(`:` ~ InfixType) )
    rule( (Bindings | Implicit | `_` ~ optional(Ascription)) ~ `=>` )
  }
  def Enumerators(G: Boolean = false): R0 = {
    def Generator: R0 = rule( Pattern1 ~ `<-` ~ Expr0(G) ~ optional(Guard(G)) )
    def Enumerator: R0 = rule( Generator | Guard(G) | Pattern1 ~ `=` ~ Expr0(G) )
    rule( Generator ~ zeroOrMore(Semis ~ Enumerator) ~ WL )
  }
  def Expr = Expr0()
  def ExprSensitive = Expr0(true)
  def Expr0(G: Boolean = false): R0 = {
    def IfCFlow = rule {
      `if` ~ '(' ~ Expr ~ ')' ~ Expr0(G) ~ optional(optional(Semi) ~ `else` ~ Expr0(G))
    }
    def WhileCFlow = rule( `while` ~ '(' ~ Expr ~ ')' ~ Expr0(G) )
    def TryCFlow = rule {
      `try` ~ Expr0(G) ~
      optional(`catch` ~ Expr0(G)) ~
      optional(`finally` ~ Expr0(G))
    }

    def DoWhileCFlow = rule {
      `do` ~ Expr0(G) ~ optional(Semi) ~ `while` ~ '(' ~ Expr ~ ")"
    }
    def ForCFlow = {

      rule {
        `for` ~
        ('(' ~ Enumerators() ~ ')' | '{' ~ Enumerators(true) ~ '}') ~
        optional(`yield`) ~
        Expr0(G)
      }
    }
    rule {
      zeroOrMore(LambdaHead) ~ (
        IfCFlow |
        WhileCFlow |
        TryCFlow |
        DoWhileCFlow |
        ForCFlow |
        `throw` ~ Expr0(G) |
        `return` ~ optional(Expr0(G)) |
        SimpleExpr() ~ `=` ~ Expr0(G) |
        PostfixExpr(G) ~ optional(`match` ~ '{' ~ CaseClauses ~ "}" | Ascription)
      )
    }
  }

  def PostfixExpr(G: Boolean = false): R0 = {
    def PrefixExpr = rule {
      optional(WL ~ anyOf("-+~!") ~ WS ~ !(Basic.OperatorChar)) ~  SimpleExpr(G)
    }
    def Check = if (G) OneNewlineMax else MATCH
    def Check0 = if (G) NotNewline else MATCH
    def InfixExpr: R0 = rule {
      PrefixExpr ~
      zeroOrMore(
        Check0 ~
        Id ~
        optional(TypeArgs) ~
        Check ~
        PrefixExpr
      )
    }
    rule( InfixExpr ~ optional(NotNewline ~ Id ~ optional(Newline)) )
  }

  def SimpleExpr(G: Boolean = false): R0 = {
    def Path: R0 = rule {
      zeroOrMore(Id ~ '.') ~ `this` ~ zeroOrMore('.' ~ Id) |
      StableId
    }
    def Check0 = if (G) NotNewline else MATCH
    def SimpleExpr1 = rule{
      `new` ~ (ClassTemplate | TemplateBody) |
      BlockExpr |
      Literal |
      Path |
      `_` |
      '(' ~ optional(Exprs) ~ ")"
    }
    rule {
      SimpleExpr1 ~
      zeroOrMore('.' ~ Id | TypeArgs | Check0 ~ ArgumentExprs) ~
      optional(Check0  ~ `_`)
    }
  }

  def Exprs: R0 = rule( oneOrMore(Expr).separatedBy(',') )
  def ArgumentExprs: R0 = rule {
    '(' ~ optional(Exprs ~ optional(`:` ~ `_*`)) ~ ")" |
      OneNewlineMax ~ BlockExpr
  }

  def BlockExpr: R0 = rule( '{' ~ (CaseClauses | Block) ~ optional(Semis) ~  "}" )

  def BlockStats: R0 = {
    def Template: R0 = rule{
      zeroOrMore(Annotation) ~
      (optional(`implicit`) ~ optional(`lazy`) ~ Def | zeroOrMore(LocalModifier) ~ TmplDef)
    }
    def BlockStat: R0 = rule {
      Import |
      Template |
      Expr0(true)
    }
    rule( oneOrMore(BlockStat).separatedBy(Semis) )
  }

  def Block: R0 = {
    def BlockEnd: R0 = rule( optional(Semis) ~ &("}" | `case`) )
    def ResultExpr: R0 = rule{ Expr0(true) | LambdaHead ~ Block}
    rule {
      zeroOrMore(LambdaHead) ~
      optional(Semis) ~
      (
        ResultExpr ~ BlockEnd |
        BlockStats ~ optional(Semis ~ ResultExpr) ~ BlockEnd |
        MATCH ~ BlockEnd
      )
    }
  }

  def CaseClauses: R0 = {
    def CaseClause: R0 = rule( `case` ~ Pattern ~ optional(Guard()) ~ `=>` ~ Block )
    rule( oneOrMore(CaseClause) )
  }

  def Guard(G: Boolean = false): R0 = rule( `if` ~ PostfixExpr(G) )
  def Pattern: R0 = rule {
    oneOrMore(Pattern1).separatedBy('|')
  }
  def Pattern1: R0 = rule( `_` ~ `:` ~ TypePat | VarId ~ `:` ~ TypePat | Pattern2 )
  def Pattern2: R0 = {
    def Pattern3: R0 = rule {
      `_*` | SimplePattern ~ zeroOrMore(Id ~ SimplePattern)
    }
    rule( VarId ~ "@" ~ Pattern3 | Pattern3 | VarId )
  }

  def SimplePattern: R0 = {

    def ExtractorArgs = rule( zeroOrMore(Pattern).separatedBy(',') )
    def Extractor: R0 = rule( StableId ~ optional('(' ~ ExtractorArgs ~ ')') )
    rule {
      `_` ~ optional(`:` ~ TypePat) ~ !"*" |
      Literal |
      '(' ~ optional(ExtractorArgs) ~ ')' |
      Extractor |
      VarId
    }
  }

  def TypeParamClause: R0 = {
    def VariantTypeParam: R0 = rule {
      zeroOrMore(Annotation) ~ optional(WL ~ anyOf("+-")) ~ TypeParam
    }
    rule( '[' ~ oneOrMore(VariantTypeParam).separatedBy(',') ~ ']' )
  }
  def FunTypeParamClause: R0 = rule {
    '[' ~ oneOrMore(zeroOrMore(Annotation) ~ TypeParam).separatedBy(',') ~ ']'
  }
  def TypeBounds: R0 = rule( optional(`>:` ~ Type) ~ optional(`<:` ~ Type) )
  def TypeParam: R0 = rule {
    (Id | `_`) ~
    optional(TypeParamClause) ~
    TypeBounds ~
    zeroOrMore(`<%` ~ Type) ~
    zeroOrMore(`:` ~ Type)
  }
  def ParamClauses: R0 = rule {
    zeroOrMore(ParamClause) ~ optional(OneNewlineMax ~ '(' ~ `implicit` ~ Params ~ ')')
  }
  def ParamClause: R0 = rule( OneNewlineMax ~ '(' ~ optional(Params) ~ ')' )
  def Params: R0 = {
    def Param: R0 = rule {
      zeroOrMore(Annotation) ~ Id ~ optional(`:` ~ ParamType) ~ optional(`=` ~ Expr)
    }
    rule( zeroOrMore(Param).separatedBy(',') )
  }

  def ClassParam: R0 = rule {
    zeroOrMore(Annotation) ~
    optional(zeroOrMore(Modifier) ~ (`val` | `var`)) ~
    Id ~
    `:` ~
    ParamType ~
    optional(`=` ~ Expr)
  }

  def Modifier: R0 = rule( LocalModifier | AccessModifier | `override` )
  def LocalModifier: R0 = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  def AccessModifier: R0 = {
    def AccessQualifier: R0 = rule( '[' ~ (`this` | Id) ~ ']' )
    rule( (`private` | `protected`) ~ optional(AccessQualifier) )
  }

  def Annotation: R0 = rule( '@' ~ !Identifiers.Operator ~ SimpleType ~  zeroOrMore(ArgumentExprs)  )

  def TemplateBody: R0 = rule {
    '{' ~
    optional(SelfType) ~
    optional(Semis) ~
    zeroOrMore(TemplateStat).separatedBy(Semis) ~
    optional(Semis) ~
    '}'
  }
  def TemplateStat: R0 = rule {
    Import |
    zeroOrMore(Annotation ~ OneNewlineMax) ~ zeroOrMore(Modifier) ~ (Def | Dcl) |
    Expr0(true)
  }

  def SelfType: R0 = rule {
    `this` ~ `:` ~ InfixType ~ `=>` | (Id | `_`) ~ optional(`:` ~ InfixType) ~ `=>`
  }

  def Import: R0 = {
    def ImportExpr: R0 = rule {
      StableId ~ optional('.' ~ (`_` | ImportSelectors))
    }
    def ImportSelectors: R0 = rule {
      '{' ~ zeroOrMore(ImportSelector ~ ',') ~ (ImportSelector | `_`) ~ "}"
    }
    def ImportSelector: R0 = rule( Id ~ optional(`=>` ~ (Id | `_`)) )
    rule( `import` ~ oneOrMore(ImportExpr).separatedBy(',') )
  }

  def Dcl: R0 = {
    def VarDcl: R0 = rule( Ids ~ `:` ~ Type )
    def FunDcl: R0 = rule( FunSig ~ optional(`:` ~ Type) )
    rule( `val` ~ ValDcl | `var` ~ VarDcl | `def` ~ FunDcl | `type` ~ TypeDcl )
  }
  def FunSig: R0 = rule( Id ~ optional(FunTypeParamClause) ~ ParamClauses )
  def ValDcl: R0 = rule( Ids ~ `:` ~ Type )
  def TypeDcl: R0 = rule( Id ~ optional(TypeParamClause) ~ TypeBounds )

  def PatVarDef: R0 = {
    def PatDef: R0 = rule( oneOrMore(Pattern2).separatedBy(',') ~ optional(`:` ~ Type) ~ `=` ~ Expr0(true) )
    def VarDef: R0 = rule( Ids ~ `:` ~ Type ~ `=` ~ `_` | PatDef )
    rule( `val` ~ PatDef | `var` ~ VarDef )
  }
  def Def: R0 = {
    def ConstrExpr: R0 = rule( ConstrBlock | SelfInvocation )
    def FunDef: R0 = rule {
      `this` ~ ParamClause ~ ParamClauses ~ (`=` ~ ConstrExpr | OneNewlineMax ~ ConstrBlock) |
      FunSig ~ optional(`:` ~ Type) ~ (
        `=` ~ optional(`macro`) ~ Expr0(true) |
        OneNewlineMax ~ '{' ~ Block ~ "}"
      )
    }
    rule( `def` ~ FunDef | `type` ~ TypeDef | PatVarDef | TmplDef )
  }

  def TypeDef: R0 = rule( Id ~ optional(TypeParamClause) ~ `=` ~ Type )

  def TmplDef: R0 = {
    def TraitTemplate: R0 = {
      def TraitParents: R0 = rule( AnnotType ~ zeroOrMore(`with` ~ AnnotType) )
      rule( optional(EarlyDefs) ~ TraitParents ~ optional(TemplateBody) )
    }
    def ClassParamClauses: R0 = {
      def Implicit: R0 = rule{
        OneNewlineMax ~
        '(' ~
        `implicit` ~
        oneOrMore(ClassParam).separatedBy(",") ~
        ")"
      }

      def ClassParamClause: R0 = {
        def ClassParams: R0 = rule( oneOrMore(ClassParam).separatedBy(',') )
        rule( OneNewlineMax ~'(' ~ optional(ClassParams) ~ ")" )
      }
      rule {
        oneOrMore(ClassParamClause) ~ optional(Implicit) | Implicit
      }
    }
    def ConstrPrelude: R0 = {
      def Annot: R0 = rule( '@' ~ SimpleType ~ ArgumentExprs )
      rule{
        NotNewline ~ (
          oneOrMore(Annot) ~ optional(AccessModifier) |
          zeroOrMore(Annot) ~ AccessModifier
        )
      }
    }
    def ClassDef: R0 = rule {
      Id ~
      optional(TypeParamClause) ~
      optional(ConstrPrelude) ~
      optional(ClassParamClauses) ~
      ClassTemplateOpt
    }
    def TraitTemplateOpt: R0 = rule {
      `extends` ~ TraitTemplate | optional(optional(`extends`) ~ TemplateBody)
    }
    def TraitDef: R0 = rule( Id ~ optional(TypeParamClause) ~ TraitTemplateOpt )
    rule {
      `trait` ~ TraitDef |
      optional(`case`) ~ (`class` ~ ClassDef | `object` ~ ObjectDef)
    }
  }


  def ObjectDef: R0 = rule( Id ~ ClassTemplateOpt )
  def ClassTemplateOpt: R0 = rule {
    `extends` ~ ClassTemplate | optional(optional(`extends`) ~ TemplateBody)
  }

  def ClassTemplate: R0 = {
    def ClassParents: R0 = {
      def Constr: R0 = rule( AnnotType ~ zeroOrMore(NotNewline ~ ArgumentExprs) )
      rule( Constr ~ zeroOrMore(`with` ~ AnnotType) )
    }
    rule( optional(EarlyDefs) ~ ClassParents ~ optional(TemplateBody) )
  }

  def EarlyDefs: R0 = {
    def EarlyDef: R0 = rule {
      zeroOrMore(Annotation ~ OneNewlineMax) ~ zeroOrMore(Modifier) ~ PatVarDef
    }
    rule( '{' ~ optional(oneOrMore(EarlyDef).separatedBy(Semis)) ~ optional(Semis) ~ '}' ~ `with` )
  }

  def ConstrBlock: R0 = rule( '{' ~ SelfInvocation ~ optional(Semis ~ BlockStats) ~ optional(Semis) ~ '}' )
  def SelfInvocation: R0 = rule( `this` ~ oneOrMore(ArgumentExprs) )

  def TopStatSeq: R0 = {
    def PackageObject: R0 = rule( `package` ~ `object` ~ ObjectDef )
    def Packaging: R0 = rule {
      `package` ~ QualId ~ '{' ~ optional(Semis) ~ optional(TopStatSeq) ~ optional(Semis) ~ '}'
    }
    def Template = rule( zeroOrMore(Annotation ~ OneNewlineMax) ~ zeroOrMore(Modifier) ~ TmplDef )
    def TopStat: R0 = rule( Packaging | PackageObject | Import | Template )
    rule( oneOrMore(TopStat).separatedBy(Semis) )
  }

  def CompilationUnit: Rule1[String] = {
    def TopPackageSeq: R0 = rule{
      oneOrMore(`package` ~ QualId ~ !(WS ~ "{")).separatedBy(Semis)
    }
    rule {
      capture(
        optional(Semis) ~
        (TopPackageSeq ~ optional(Semis ~ TopStatSeq) | TopStatSeq | MATCH) ~
        optional(Semis) ~
        WL
      )
    }
  }
}
