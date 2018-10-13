package scalaparse

import language.implicitConversions
import syntax._
import fastparse._
import fastparse._, ScalaWhitespace._
/**
 * Parser for Scala syntax.
 */
object Scala extends Core with Types with Exprs with Xml{

  def TmplBody[_: P]: P[Unit] = {
    def Prelude = P( (Annot ~ OneNLMax).rep ~ Mod./.rep )
    def TmplStat = P( Import | Prelude ~ BlockDef | StatCtx.Expr )

    P( "{" ~/ BlockLambda.? ~ Semis.? ~ TmplStat.repX(sep = NoCut(Semis)) ~ Semis.? ~ `}` )
  }

  def ValVarDef[_: P] = P( BindPattern.rep(1, ","./) ~ (`:` ~/ Type).? ~ (`=` ~/ FreeCtx.Expr).? )

  def FunDef[_: P] = {
    def Body = P( WL ~ `=` ~/ `macro`.? ~ StatCtx.Expr | OneNLMax ~ "{" ~ Block ~ "}" )
    P( FunSig ~ (`:` ~/ Type).? ~~ Body.? )
  }

  def BlockDef[_: P]: P[Unit] = P( Dcl | TraitDef  | ClsDef | ObjDef )

  def ClsDef[_: P] = {
    def ClsAnnot = P( `@` ~ SimpleType ~ ArgList.? )
    def Prelude = P( NotNewline ~ ( ClsAnnot.rep(1) ~ AccessMod.? | AccessMod) )
    def ClsArgMod = P( Mod.rep ~ (`val` | `var`) )
    def ClsArg = P( Annot.rep ~ ClsArgMod.? ~ Id ~ `:` ~ Type ~ (`=` ~ ExprCtx.Expr).? )

    def ClsArgs = P( OneNLMax ~ "(" ~/ `implicit`.? ~ ClsArg.repTC() ~ ")" )
    P( `case`.? ~ `class` ~/ Id ~ TypeArgList.? ~~ Prelude.? ~~ ClsArgs.repX ~ DefTmpl.? )
  }

  def Constrs[_: P] = P( (WL ~ Constr).rep(1, `with`./) )
  def EarlyDefTmpl[_: P] = P( TmplBody ~ (`with` ~/ Constr).rep ~ TmplBody.? )
  def NamedTmpl[_: P] = P( Constrs ~ TmplBody.? )

  def DefTmpl[_: P] = P( (`extends` | `<:`) ~ AnonTmpl | TmplBody )
  def AnonTmpl[_: P] = P( EarlyDefTmpl | NamedTmpl | TmplBody )

  def TraitDef[_: P] = P( `trait` ~/ Id ~ TypeArgList.? ~ DefTmpl.? )

  def ObjDef[_: P]: P[Unit] = P( `case`.? ~ `object` ~/ Id ~ DefTmpl.? )

  def Constr[_: P] = P( AnnotType ~~ (NotNewline ~ ParenArgList ).repX )

  def PkgObj[_: P] = P( ObjDef )
  def PkgBlock[_: P] = P( QualId ~/ `{` ~ TopStatSeq.? ~ `}` )
  def Pkg[_: P] = P( `package` ~/ (PkgBlock | PkgObj) )
  def TopStatSeq[_: P]: P[Unit] = {
    def Tmpl = P( (Annot ~~ OneNLMax).rep ~ Mod.rep ~ (TraitDef | ClsDef | ObjDef) )
    def TopStat = P( Pkg | Import | Tmpl )
    P( TopStat.repX(1, Semis) )
  }
  def TopPkgSeq[_: P] = P( ((`package` ~ QualId) ~~ !(WS ~ "{")).repX(1, Semis) )
  def CompilationUnit[_: P]: P[Unit] = {
    def Body = P( TopPkgSeq ~~ (Semis ~ TopStatSeq).? | TopStatSeq )
    P( Semis.? ~ Body.? ~~ Semis.? ~ WL0 ~ End )
  }
}
