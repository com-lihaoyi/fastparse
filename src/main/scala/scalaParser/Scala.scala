package scalaParser
import acyclic.file
import language.implicitConversions
import syntax._
import org.parboiled2._
import macros.Macros._
/**
 * Parser for Scala syntax.
 */
class Scala (val input: ParserInput)
  extends Core with Types with Exprs with Xml{

  import KeyWordOperators._
  import KeyWordOperators.`_`
  private implicit def wspStr(s: String) = rule( WL ~ str(s) )
  private implicit def wspCh(s: Char) = rule( WL ~ ch(s) )

  def TmplBody: R0 = {
    def Prelude = rule( rep(Annot ~ OneNLMax) ~ rep(Mod) )
    def TmplStat = rule( Import | Prelude ~ (Def | Dcl) | Expr0(true) )
    def ThisType = rule( `this` ~ `:` ~ InfixType ~ `=>` )
    def OtherType = rule( (Id | `_`) ~ opt(`:` ~ InfixType) ~ `=>` )
    def SelfType = rule( ThisType | OtherType)
    rule( '{' ~ opt(SelfType) ~ opt(Semis) ~ repSep(TmplStat, Semis) ~ `}` )
  }

  def NewBody = rule( ClsTmpl | TmplBody )

  def BlockDef = rule( Def | TmplDef )

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
