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
    def TmplStat = rule( Import | Prelude ~ (BlockDef | Dcl) | StatCtx.Expr )
    def SelfType = rule( (`this` | Id | `_`) ~ (`:` ~ InfixType).? ~ `=>` )
    rule( '{' ~ SelfType.? ~ Semis.? ~ TmplStat.*(Semis) ~ `}` )
  }

  def NewBody = rule( ClsTmpl | TmplBody )

  def ValRhs = rule( Pat2.+(',') ~ (`:` ~ Type).? ~ `=` ~ StatCtx.Expr )
  def ValDef = rule( `val` ~ ValRhs )
  def VarDef = rule( `var` ~ Ids ~ `:` ~ Type ~ `=` ~ `_` | `var` ~ ValRhs )

  def DefDef = {
    def Body = rule( `=` ~ `macro`.? ~ StatCtx.Expr | OneNLMax ~ '{' ~ Block ~ "}" )
    rule( `def` ~ FunSig ~ (`:` ~ Type).? ~ Body )
  }

  def BlockDef: R0 = rule( DefDef | TypeDef | ValDef | VarDef | TraitDef | ClsDef | ObjDef )

  def ClsDef = {
    def ClsAnnot = rule( `@` ~ SimpleType ~ ArgList )
    def Prelude = rule( NotNewline ~ ( ClsAnnot.+ ~ AccessMod.? | ClsAnnot.* ~ AccessMod) )
    def ClsArgMod = rule( (Mod.* ~ (`val` | `var`)).? )
    def ClsArg = rule( Annot.* ~ ClsArgMod ~ Id ~ `:` ~ ParamType ~ (`=` ~ ExprCtx.Expr).? )

    def Implicit = rule( OneNLMax ~ '(' ~ `implicit` ~ ClsArg.+(",") ~ ")" )
    def ClsArgs = rule( OneNLMax ~'(' ~ ClsArg.*(',') ~ ")" )
    def AllArgs = rule( ClsArgs.+ ~ Implicit.? | Implicit )
    rule( `case`.? ~ `class` ~ Id ~ TypeArgList.? ~ Prelude.? ~ AllArgs.? ~ ClsTmplOpt )
  }
  def TraitDef = {
    def TraitTmplOpt = {
      def TraitParents = rule( AnnotType ~ (`with` ~ AnnotType).* )
      def TraitTmpl = rule( EarlyDefs.? ~ TraitParents ~ TmplBody.? )
      rule( `extends` ~ TraitTmpl | (`extends`.? ~ TmplBody).? )
    }
    rule( `trait` ~ Id ~ TypeArgList.? ~ TraitTmplOpt )
  }

  def ObjDef: R0 = rule( `case`.? ~ `object` ~ Id ~ ClsTmplOpt )
  def ClsTmplOpt: R0 = rule( `extends` ~ ClsTmpl | (`extends`.? ~ TmplBody).? )

  def ClsTmpl: R0 = {
    def Constr = rule( AnnotType ~ (NotNewline ~ ArgList).* )
    def ClsParents = rule( Constr ~ (`with` ~ AnnotType).* )
    rule( EarlyDefs.? ~ ClsParents ~ TmplBody.? )
  }

  def EarlyDefs: R0 = {
    def EarlyDef = rule( (Annot ~ OneNLMax).* ~ Mod.* ~ (ValDef | VarDef) )
    rule( `{` ~ EarlyDef.*(Semis) ~ `}` ~ `with` )
  }

  def PkgObj = rule( `package` ~ ObjDef )
  def PkgBlock = rule( `package` ~ QualId ~ `{` ~ TopStatSeq.? ~ `}` )
  def TopStatSeq: R0 = {
    def Tmpl = rule( (Annot ~ OneNLMax).* ~ Mod.* ~ (TraitDef | ClsDef | ObjDef) )
    def TopStat = rule( PkgBlock | PkgObj | Import | Tmpl )
    rule( TopStat.+(Semis) )
  }

  def CompilationUnit: Rule1[String] = {
    def TopPackageSeq = rule( (`package` ~ QualId ~ !(WS ~ "{")).+(Semis) )
    def Body = rule( TopPackageSeq ~ (Semis ~ TopStatSeq).? | TopStatSeq | MATCH )
    rule( capture(Semis.? ~ Body ~ Semis.? ~ WL) )
  }
}
