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
  def WS = rule { zeroOrMore(Basic.WhitespaceChar | Literals.Comment) }

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL = rule{ zeroOrMore(Basic.WhitespaceChar | Literals.Comment | Basic.Newline) }


  /**
   * By default, all strings and characters greedily
   * capture all whitespace immediately before the token.
   */
  implicit private[this] def wspStr(s: String): R0 = rule { WL ~ str(s)  }
  implicit private[this] def wspChar(s: Char): R0 = rule { WL ~ ch(s) }

  /**
   * Most keywords don't just require the correct characters to match,
   * they have to ensure that subsequent characters *don't* match in
   * order for it to be a keyword. This enforces that rule for key-words
   * (W) and key-operators (O) which have different non-match criteria.
   */
  object K {
    def W(s: String) = rule{ WL ~ Key.W(s) }
    def O(s: String) = rule{ WL ~ Key.O(s) }
  }

  def `=>` = rule{ K.O("=>") | K.O("⇒") }
  def `:` = rule{ K.O(":") }
  def `=` = rule{ K.O("=") }

  def `_` = rule{ K.W("_") }
  def `this` = rule{ K.W("this") }
  def `type` = rule{ K.W("type") }
  def `val` = rule{ K.W("val") }
  def `var` = rule{ K.W("var") }
  def `def` = rule{ K.W("def") }
  def `with` = rule{ K.W("with") }
  def `package` = rule{ K.W("package") }
  def `object` = rule{ K.W("object") }
  def `class` = rule{ K.W("class") }
  def `case` = rule{ K.W("case") }
  def `trait` = rule{ K.W("trait") }
  def `extends` = rule{ K.W("extends") }
  def `implicit` = rule{ K.W("implicit") }

  /**
   * helper printing function
   */
  def pr(s: String) = rule { run(println(s"LOGGING $cursor: $s")) }

  def Id = rule { WL ~ Identifiers.Id }
  def VarId = rule { WL ~ Identifiers.VarId }
  def Literal = rule { WL ~ Literals.Literal }
  def Semi = rule { WS ~ Basic.Semi }
  def Semis = rule { oneOrMore(Semi) }
  def Newline = rule { WL ~ Basic.Newline }

  def QualId = rule { WL ~ oneOrMore(Id).separatedBy('.') }
  def Ids = rule { oneOrMore(Id) separatedBy ',' }

  def NotNewline: R0 = rule{ &( WS ~ !Basic.Newline )}
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
    def ClassQualifier = rule { '[' ~ Id ~ ']' }
    rule {
      zeroOrMore(Id ~ '.') ~ (`this` | K.W("super") ~ optional(ClassQualifier)) ~ zeroOrMore('.' ~ Id) |
      Id ~ zeroOrMore('.' ~ Id)
    }
  }
  def ExistentialDcl = rule { `type` ~ TypeDcl | `val` ~ ValDcl }
  def ExistentialClause = rule {
    "forSome" ~ '{' ~ oneOrMore(ExistentialDcl).separatedBy(Semi) ~ '}'
  }
  def Type: R0 = {
    def FunctionArgTypes = rule {
      '(' ~ optional(oneOrMore(ParamType) separatedBy ',') ~ ')'
    }
    rule {
      (
        `_` |
        FunctionArgTypes ~ `=>` ~ Type |
        InfixType ~ (
          `=>` ~ Type |
          optional(ExistentialClause)
        )
      ) ~ TypeBounds
    }
  }

  def InfixType = rule {
    CompoundType ~ zeroOrMore(NotNewline ~ Id ~ OneNewlineMax ~ CompoundType)
  }
  def CompoundType = {
    def RefineStat = rule { `type` ~ TypeDef | Dcl | MATCH }
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
  def AnnotType = rule {
    SimpleType ~ optional(NotNewline ~ oneOrMore(NotNewline ~ Annotation))
  }
  def SimpleType: R0 = {
    def BasicType: R0 = rule {
      '(' ~ Types ~ ')'  | StableId ~ '.' ~ `type` | StableId
    }
    rule{ BasicType ~ zeroOrMore(TypeArgs | '#' ~ Id) }
  }

  def TypeArgs = rule { '[' ~ Types ~ "]" }
  def Types = rule { oneOrMore(Type).separatedBy(',') }


  def TypePat = rule { CompoundType }

  def Ascription = rule {
     ":" ~ ("_" ~ "*" |  Type | oneOrMore(Annotation))
  }

  def ParamType = rule { `=>` ~ Type | Type ~ "*" | Type }

  def LambdaHead: R0 = {
    def Bindings: R0 = {
      def Binding: R0 = rule { (Id | `_`) ~ optional(`:` ~ Type) }
      rule { '(' ~ zeroOrMore(Binding).separatedBy(',') ~ ')' }
    }
    rule{
      (
        Bindings |
        optional(`implicit`) ~ Id ~ optional(":" ~ InfixType) |
        `_` ~ optional(Ascription)
      ) ~
      `=>`
    }
  }
  def Enumerators(G: Boolean = false): R0 = {
    def Generator: R0 = rule {
      Pattern1 ~ (K.O("<-") | K.O("←"))~ Expr0(G) ~ optional(Guard(G))
    }
    def Enumerator: R0 = rule { Generator | Guard(G) | Pattern1 ~ `=` ~ Expr0(G) }
    rule { Generator ~ zeroOrMore(Semis ~ Enumerator) ~ WL }
  }
  def Expr = Expr0()
  def ExprSensitive = Expr0(true)
  def Expr0(G: Boolean = false): R0 = {
    def IfCFlow = rule {
      "if" ~ '(' ~ Expr ~ ')' ~ Expr0(G) ~ optional(optional(Semi) ~ K.W("else") ~ Expr0(G))
    }
    def WhileCFlow = rule { "while" ~ '(' ~ Expr ~ ')' ~ Expr0(G) }
    def TryCFlow = rule {
      K.W("try") ~ Expr0(G) ~
      optional(K.W("catch") ~ Expr0(G)) ~
      optional(K.W("finally") ~ Expr0(G))
    }

    def DoWhileCFlow = rule {
      K.W("do") ~ Expr0(G) ~ optional(Semi) ~ "while" ~ '(' ~ Expr ~ ")"
    }
    def ForCFlow = {

      rule {
        "for" ~
        ('(' ~ Enumerators() ~ ')' | '{' ~ Enumerators(true) ~ '}') ~
        optional(K.W("yield")) ~
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
        K.W("throw") ~ Expr0(G) |
        K.W("return") ~ optional(Expr0(G)) |
        SimpleExpr() ~ `=` ~ Expr0(G) |
        PostfixExpr(G) ~ optional("match" ~ '{' ~ CaseClauses ~ "}" | Ascription)
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
    rule { InfixExpr ~ optional(NotNewline ~ Id ~ optional(Newline)) }
  }

  def SimpleExpr(G: Boolean = false): R0 = {
    def Path: R0 = rule {
      zeroOrMore(Id ~ '.') ~ `this` ~ zeroOrMore('.' ~ Id) |
      StableId
    }
    def Check0 = if (G) NotNewline else MATCH
    def SimpleExpr1 = rule{
      K.W("new") ~ (ClassTemplate | TemplateBody) |
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

  def Exprs: R0 = rule { oneOrMore(Expr).separatedBy(',') }
  def ArgumentExprs: R0 = rule {
    '(' ~ optional(Exprs ~ optional(`:` ~ `_` ~ '*')) ~ ")" |
      OneNewlineMax ~ BlockExpr
  }

  def BlockExpr: R0 = rule { '{' ~ (CaseClauses | Block) ~ optional(Semis) ~  "}" }

  def BlockStats: R0 = {
    def Template: R0 = rule{
      zeroOrMore(Annotation) ~
      (optional(`implicit`) ~ optional(K.W("lazy")) ~ Def | zeroOrMore(LocalModifier) ~ TmplDef)
    }
    def BlockStat: R0 = rule {
      Import |
      Template |
      Expr0(true)
    }
    rule{ oneOrMore(BlockStat).separatedBy(Semis) }
  }

  def Block: R0 = {
    def BlockEnd: R0 = rule{ optional(Semis) ~ &("}" | `case`) }
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
    def CaseClause: R0 = rule { `case` ~ Pattern ~ optional(Guard()) ~ `=>` ~ Block }
    rule { oneOrMore(CaseClause) }
  }

  def Guard(G: Boolean = false): R0 = rule { K.W("if") ~ PostfixExpr(G) }
  def Pattern: R0 = rule {
    oneOrMore(Pattern1).separatedBy('|')
  }
  def Pattern1: R0 = rule { `_` ~ `:` ~ TypePat | VarId ~ `:` ~ TypePat | Pattern2 }
  def Pattern2: R0 = {
    def Pattern3: R0 = rule {
      `_` ~ '*' | SimplePattern ~ zeroOrMore(Id ~ SimplePattern)
    }
    rule{ VarId ~ "@" ~ Pattern3 | Pattern3 | VarId }
  }

  def SimplePattern: R0 = {

    def ExtractorArgs = rule{ zeroOrMore(Pattern).separatedBy(',') }
    def Extractor: R0 = rule{ StableId ~ optional('(' ~ ExtractorArgs ~ ')') }
    rule {
      `_` ~ optional(`:` ~ TypePat) ~ !("*") |
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
    rule { '[' ~ oneOrMore(VariantTypeParam).separatedBy(',') ~ ']' }
  }
  def FunTypeParamClause: R0 = rule {
    '[' ~ oneOrMore(zeroOrMore(Annotation) ~ TypeParam).separatedBy(',') ~ ']'
  }
  def TypeBounds: R0 = rule{ optional(K.O(">:") ~ Type) ~ optional(K.O("<:") ~ Type) }
  def TypeParam: R0 = rule {
    (Id | `_`) ~
    optional(TypeParamClause) ~
    TypeBounds ~
    zeroOrMore(K.O("<%") ~ Type) ~
    zeroOrMore(`:` ~ Type)
  }
  def ParamClauses: R0 = rule {
    zeroOrMore(ParamClause) ~ optional(OneNewlineMax ~ '(' ~ `implicit` ~ Params ~ ')')
  }
  def ParamClause: R0 = rule { OneNewlineMax ~ '(' ~ optional(Params) ~ ')' }
  def Params: R0 = {
    def Param: R0 = rule {
      zeroOrMore(Annotation) ~ Id ~ optional(`:` ~ ParamType) ~ optional(`=` ~ Expr)
    }
    rule { zeroOrMore(Param).separatedBy(',') }
  }

  def ClassParam: R0 = rule {
    zeroOrMore(Annotation) ~
    optional(zeroOrMore(Modifier) ~ (`val` | `var`)) ~
    Id ~
    `:` ~
    ParamType ~
    optional(`=` ~ Expr)
  }

  def Modifier: R0 = rule { LocalModifier | AccessModifier | K.W("override") }
  def LocalModifier: R0 = rule { K.W("abstract") | K.W("final") | K.W("sealed") | `implicit` | K.W("lazy") }
  def AccessModifier: R0 = {
    def AccessQualifier: R0 = rule { '[' ~ (`this` | Id) ~ ']' }
    rule { (K.W("private") | K.W("protected")) ~ optional(AccessQualifier) }
  }

  def Annotation: R0 = rule {  '@' ~ !Identifiers.Operator ~ SimpleType ~  zeroOrMore(ArgumentExprs)  }

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
    def ImportSelector: R0 = rule { Id ~ optional(`=>` ~ (Id | `_`)) }
    rule { K.W("import") ~ oneOrMore(ImportExpr).separatedBy(',') }
  }

  def Dcl: R0 = {
    def VarDcl: R0 = rule { Ids ~ `:` ~ Type }
    def FunDcl: R0 = rule { FunSig ~ optional(`:` ~ Type) }
    rule{ `val` ~ ValDcl | `var` ~ VarDcl | `def` ~ FunDcl | `type` ~ TypeDcl }
  }
  def FunSig: R0 = rule { Id ~ optional(FunTypeParamClause) ~ ParamClauses }
  def ValDcl: R0 = rule { Ids ~ `:` ~ Type }
  def TypeDcl: R0 = rule { Id ~ optional(TypeParamClause) ~ TypeBounds }

  def PatVarDef: R0 = {
    def PatDef: R0 = rule { oneOrMore(Pattern2).separatedBy(',') ~ optional(`:` ~ Type) ~ `=` ~ Expr0(true) }
    def VarDef: R0 = rule { Ids ~ `:` ~ Type ~ `=` ~ `_` | PatDef }
    rule { `val` ~ PatDef | `var` ~ VarDef }
  }
  def Def: R0 = {
    def ConstrExpr: R0 = rule { ConstrBlock | SelfInvocation }
    def FunDef: R0 = rule {
      `this` ~ ParamClause ~ ParamClauses ~ (`=` ~ ConstrExpr | OneNewlineMax ~ ConstrBlock) |
      FunSig ~ optional(`:` ~ Type) ~ (
        `=` ~ optional(K.W("macro")) ~ Expr0(true) |
        OneNewlineMax ~ '{' ~ Block ~ "}"
      )
    }
    rule { `def` ~ FunDef | `type` ~ TypeDef | PatVarDef | TmplDef }
  }

  def TypeDef: R0 = rule { Id ~ optional(TypeParamClause) ~ `=` ~ Type }

  def TmplDef: R0 = {
    def TraitTemplate: R0 = {
      def TraitParents: R0 = rule { AnnotType ~ zeroOrMore(`with` ~ AnnotType) }
      rule{ optional(EarlyDefs) ~ TraitParents ~ optional(TemplateBody) }
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
        def ClassParams: R0 = rule { oneOrMore(ClassParam).separatedBy(',') }
        rule { OneNewlineMax ~'(' ~ optional(ClassParams) ~ ")" }
      }
      rule {
        oneOrMore(ClassParamClause) ~ optional(Implicit) | Implicit
      }
    }
    def ConstrPrelude: R0 = {
      def Annot: R0 = rule { '@' ~ SimpleType ~ ArgumentExprs }
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
    def TraitDef: R0 = rule { Id ~ optional(TypeParamClause) ~ TraitTemplateOpt }
    rule {
      `trait` ~ TraitDef |
      optional(`case`) ~ (`class` ~ ClassDef | `object` ~ ObjectDef)
    }
  }


  def ObjectDef: R0 = rule { Id ~ ClassTemplateOpt }
  def ClassTemplateOpt: R0 = rule {
    `extends` ~ ClassTemplate | optional(optional(`extends`) ~ TemplateBody)
  }

  def ClassTemplate: R0 = {
    def ClassParents: R0 = {
      def Constr: R0 = rule{ AnnotType ~ zeroOrMore(NotNewline ~ ArgumentExprs) }
      rule{ Constr ~ zeroOrMore(`with` ~ AnnotType) }
    }
    rule{ optional(EarlyDefs) ~ ClassParents ~ optional(TemplateBody) }
  }

  def EarlyDefs: R0 = {
    def EarlyDef: R0 = rule {
      zeroOrMore(Annotation ~ OneNewlineMax) ~ zeroOrMore(Modifier) ~ PatVarDef
    }
    rule{ '{' ~ optional(oneOrMore(EarlyDef).separatedBy(Semis)) ~ optional(Semis) ~ '}' ~ `with` }
  }

  def ConstrBlock: R0 = rule { '{' ~ SelfInvocation ~ optional(Semis ~ BlockStats) ~ optional(Semis) ~ '}' }
  def SelfInvocation: R0 = rule { `this` ~ oneOrMore(ArgumentExprs) }

  def TopStatSeq: R0 = {
    def PackageObject: R0 = rule { `package` ~ `object` ~ ObjectDef }
    def Packaging: R0 = rule {
      `package` ~ QualId ~ '{' ~ optional(Semis) ~ optional(TopStatSeq) ~ optional(Semis) ~ '}'
    }
    def TopStat: R0 = rule {
      Packaging |
      PackageObject |
      Import |
      zeroOrMore(Annotation ~ OneNewlineMax) ~ zeroOrMore(Modifier) ~ TmplDef
    }
    rule { oneOrMore(TopStat).separatedBy(Semis) }
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
