package scalaparse

import acyclic.file
import language.implicitConversions
import syntax._
import fastparse._
/**
 * Parser for Scala syntax.
 */
object Scala extends Core with Types with Exprs with Xml{

  private implicit def wspStr(s: String) = P(WL ~ s)(Utils.literalize(s).toString)


  val TmplBody: P0 = {
    val Prelude = P( (Annot ~ OneNLMax).rep ~ (Mod ~! Pass).rep )
    val TmplStat = P( WL ~ (Import | Prelude ~ BlockDef | StatCtx.Expr) )

    P( "{" ~! BlockLambda.? ~ Semis.? ~ TmplStat.rep(sep = Semis) ~ `}` )
  }

  val ValVarDef = P( BindPattern.rep(1, "," ~!) ~ (`:` ~! Type).? ~ (`=` ~! StatCtx.Expr).? )

  val FunDef = {
    val Body = P( `=` ~! `macro`.? ~ StatCtx.Expr | OneNLMax ~ "{" ~ Block ~ "}" )
    P( FunSig ~ (`:` ~! Type).? ~ Body.? )
  }

  val BlockDef: P0 = P( WL ~ (Dcl | TraitDef | ClsDef | ObjDef) )

  val ClsDef = {
    val ClsAnnot = P( `@` ~ SimpleType ~ ArgList.? )
    val Prelude = P( NotNewline ~ ( ClsAnnot.rep(1) ~ AccessMod.? | ClsAnnot.rep ~ AccessMod) )
    val ClsArgMod = P( Mod.rep ~ (`val` | `var`) )
    val ClsArg = P( Annot.rep ~ ClsArgMod.? ~ Id ~ `:` ~ Type ~ (`=` ~ ExprCtx.Expr).? )

    val ClsArgs = P( OneNLMax ~ "(" ~! `implicit`.? ~ ClsArg.rep(sep = "," ~!, end = ")") )
    val AllArgs = P( ClsArgs.rep)
    P( `case`.? ~ `class` ~! Id ~ TypeArgList.? ~ Prelude.? ~ AllArgs ~ DefTmpl.? )
  }

  val Constrs = P( Constr.rep(1, `with` ~!) )
  val EarlyDefTmpl = P( TmplBody ~ (`with` ~! Constr).rep ~ TmplBody.? )
  val NamedTmpl = P( Constrs ~ TmplBody.? )

  val DefTmpl = P( (`extends` | `<:`) ~ AnonTmpl | TmplBody)
  val AnonTmpl = P( EarlyDefTmpl | NamedTmpl | TmplBody )

  val TraitDef = P( `trait` ~! Id ~ TypeArgList.? ~ DefTmpl.? )

  val ObjDef: P0 = P( `case`.? ~ `object` ~! Id ~ DefTmpl.? )

  val Constr = P( AnnotType ~ (NotNewline ~ ParenArgList ).rep )

  val PkgObj = P( ObjDef )
  val PkgBlock = P( QualId ~! `{` ~ TopStatSeq.? ~ `}` )
  val TopStatSeq: P0 = {
    val Tmpl = P( (Annot ~ OneNLMax).rep ~ Mod.rep ~ (TraitDef | ClsDef | ObjDef) )
    val TopStat = P( `package` ~! (PkgBlock | PkgObj) | Import | Tmpl )
    P( (WL ~ TopStat).rep(1, Semis) )
  }
  val TopPkgSeq = P( (`package` ~ QualId ~ !(WS ~ "{")).rep(1, Semis) )
  val CompilationUnit: P0 = {
    val Body = P( TopPkgSeq ~ (Semis ~ TopStatSeq).? | TopStatSeq )
    P( Semis.? ~ Body.? ~ Semis.? ~ WL ~ End)
  }
}
