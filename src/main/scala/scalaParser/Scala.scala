package scalaParser
import acyclic.file
import language.implicitConversions
import syntax._
import org.parboiled2._

/**
 * Parser for Scala syntax.
 */
class Scala (val input: ParserInput)
  extends Core with Types with Exprs with Xml{

  private implicit def wspStr(s: String) = rule( WL ~ str(s) )
  private implicit def wspCh(s: Char) = rule( WL ~ ch(s) )

  def TmplBody: R0 = {
    def Prelude = rule( (Annot ~ OneNLMax).* ~ Mod.* )
    def TmplStat = rule( Import | Prelude ~ (Def | Dcl) | StatCtx.Expr )
    def SelfType = rule( (`this` | Id | `_`) ~ (`:` ~ InfixType).? ~ `=>` )
    rule( '{' ~ SelfType.? ~ Semis.? ~ TmplStat.*(Semis) ~ `}` )
  }

  def NewBody = rule( ClsTmpl | TmplBody )

  def BlockDef = rule( Def | TmplDef )

  def ValVarDef: R0 = {
    def Val = rule( Pat2.+(',') ~ (`:` ~ Type).? ~ `=` ~ StatCtx.Expr )
    def Var = rule( Ids ~ `:` ~ Type ~ `=` ~ `_` | Val )
    rule( `val` ~ Val | `var` ~ Var )
  }
  def Def: R0 = {
    def Body = rule( `=` ~ `macro`.? ~ StatCtx.Expr | OneNLMax ~ '{' ~ Block ~ "}" )
    def FunDef = rule( `def` ~ FunSig ~ (`:` ~ Type).? ~ Body )
    rule( FunDef | TypeDef | ValVarDef | TmplDef )
  }

  def TmplDef: R0 = {
    def ClsDef = {
      def ClsAnnot = rule( `@` ~ SimpleType ~ ArgList )
      def Prelude = rule( NotNewline ~ ( ClsAnnot.+ ~ AccessMod.? | ClsAnnot.* ~ AccessMod) )
      def ClsArgMod = rule( (Mod.* ~ (`val` | `var`)).? )
      def ClsArg = rule( Annot.* ~ ClsArgMod ~ Id ~ `:` ~ ParamType ~ (`=` ~ ExprCtx.Expr).? )

      def Implicit = rule( OneNLMax ~ '(' ~ `implicit` ~ ClsArg.+(",") ~ ")" )
      def ClsArgs = rule( OneNLMax ~'(' ~ ClsArg.*(',') ~ ")" )
      def AllArgs = rule( ClsArgs.+ ~ Implicit.? | Implicit )
      rule( `class` ~ Id ~ TypeArgList.? ~ Prelude.? ~ AllArgs.? ~ ClsTmplOpt )
    }
    def TraitTmplOpt = {
      def TraitParents = rule( AnnotType ~ (`with` ~ AnnotType).* )
      def TraitTmpl = rule( EarlyDefs.? ~ TraitParents ~ TmplBody.? )
      rule( `extends` ~ TraitTmpl | (`extends`.? ~ TmplBody).? )
    }
    def TraitDef = rule( `trait` ~ Id ~ TypeArgList.? ~ TraitTmplOpt )
    rule( TraitDef | `case`.? ~ (ClsDef | ObjDef) )
  }

  def ObjDef: R0 = rule( `object` ~ Id ~ ClsTmplOpt )
  def ClsTmplOpt: R0 = rule( `extends` ~ ClsTmpl | (`extends`.? ~ TmplBody).? )

  def ClsTmpl: R0 = {
    def Constr = rule( AnnotType ~ (NotNewline ~ ArgList).* )
    def ClsParents = rule( Constr ~ (`with` ~ AnnotType).* )
    rule( EarlyDefs.? ~ ClsParents ~ TmplBody.? )
  }

  def EarlyDefs: R0 = {
    def EarlyDef = rule( (Annot ~ OneNLMax).* ~ Mod.* ~ ValVarDef )
    rule( `{` ~ EarlyDef.*(Semis) ~ `}` ~ `with` )
  }

  def TopStatSeq: R0 = {
    def PkgObj = rule( `package` ~ ObjDef )
    def PkgBlock = rule( `package` ~ QualId ~ `{` ~ TopStatSeq.? ~ `}` )
    def Tmpl = rule( (Annot ~ OneNLMax).* ~ Mod.* ~ TmplDef )
    def TopStat = rule( PkgBlock | PkgObj | Import | Tmpl )
    rule( TopStat.+(Semis) )
  }

  def CompilationUnit: Rule1[String] = {
    def TopPackageSeq = rule( (`package` ~ QualId ~ !(WS ~ "{")).+(Semis) )
    def Body = rule( TopPackageSeq ~ (Semis ~ TopStatSeq).? | TopStatSeq | MATCH )
    rule( capture(Semis.? ~ Body ~ Semis.? ~ WL) )
  }
}
