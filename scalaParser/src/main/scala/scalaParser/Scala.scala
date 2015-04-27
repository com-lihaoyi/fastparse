package scalaParser
import acyclic.file
import language.implicitConversions
import syntax._
import parsing.Parsing._
/**
 * Parser for Scala syntax.
 */
object Scala extends Core with Types with Exprs/* with Xml*/{

  private implicit def wspStr(s: String) = WL ~ s
  private implicit def wspCh(s: Char) = WL ~ s

  def TmplBody: R0 = {
    val Prelude = rule( (Annot ~ OneNLMax).rep ~ Mod.rep )
    val TmplStat = rule( Import | Prelude ~ (BlockDef | Dcl) | StatCtx.Expr )
    val SelfType = rule( (`this` | Id | `_`) ~ (`:` ~ InfixType).? ~ `=>` )
    rule( '{' ~ SelfType.? ~ Semis.? ~ TmplStat.rep(Semis) ~ `}` )
  }

  val NewBody = rule( ClsTmpl | TmplBody )

  val ValRhs = rule( Pat2.rep1(',') ~ (`:` ~ Type).? ~ `=` ~ StatCtx.Expr )
  val ValDef = rule( `val` ~ ValRhs )
  val VarDef = rule( `var` ~ Ids ~ `:` ~ Type ~ `=` ~ `_` | `var` ~ ValRhs )

  val DefDef = {
    val Body = rule( `=` ~ `macro`.? ~ StatCtx.Expr | OneNLMax ~ '{' ~ Block ~ "}" )
    rule( `def` ~ FunSig ~ (`:` ~ Type).? ~ Body )
  }

  val BlockDef: R0 = rule( DefDef | TypeDef | ValDef | VarDef | TraitDef | ClsDef | ObjDef )

  val ClsDef = {
    val ClsAnnot = rule( `@` ~ SimpleType ~ ArgList )
    val Prelude = rule( NotNewline ~ ( ClsAnnot.rep1 ~ AccessMod.? | ClsAnnot.rep ~ AccessMod) )
    val ClsArgMod = rule( (Mod.rep ~ (`val` | `var`)).? )
    val ClsArg = rule( Annot.rep ~ ClsArgMod ~ Id ~ `:` ~ ParamType ~ (`=` ~ ExprCtx.Expr).? )

    val Implicit = rule( OneNLMax ~ '(' ~ `implicit` ~ ClsArg.+(",") ~ ")" )
    val ClsArgs = rule( OneNLMax ~'(' ~ ClsArg.rep(',') ~ ")" )
    val AllArgs = rule( ClsArgs.rep1 ~ Implicit.? | Implicit )
    rule( `case`.? ~ `class` ~ Id ~ TypeArgList.? ~ Prelude.? ~ AllArgs.? ~ ClsTmplOpt )
  }
  val TraitDef = {
    val TraitTmplOpt = {
      val TraitParents = rule( AnnotType ~ (`with` ~ AnnotType).rep )
      val TraitTmpl = rule( EarlyDefs.? ~ TraitParents ~ TmplBody.? )
      rule( `extends` ~ TraitTmpl | (`extends`.? ~ TmplBody).? )
    }
    rule( `trait` ~ Id ~ TypeArgList.? ~ TraitTmplOpt )
  }

  val ObjDef: R0 = rule( `case`.? ~ `object` ~ Id ~ ClsTmplOpt )
  val ClsTmplOpt: R0 = rule( `extends` ~ ClsTmpl | (`extends`.? ~ TmplBody).? )

  val ClsTmpl: R0 = {
    val Constr = rule( AnnotType ~ (NotNewline ~ ArgList).rep )
    val ClsParents = rule( Constr ~ (`with` ~ AnnotType).rep )
    rule( EarlyDefs.? ~ ClsParents ~ TmplBody.? )
  }

  val EarlyDefs: R0 = {
    val EarlyDef = rule( (Annot ~ OneNLMax).rep ~ Mod.rep ~ (ValDef | VarDef) )
    rule( `{` ~ EarlyDef.rep(Semis) ~ `}` ~ `with` )
  }

  val PkgObj = rule( `package` ~ ObjDef )
  val PkgBlock = rule( `package` ~ QualId ~ `{` ~ TopStatSeq.? ~ `}` )
  val TopStatSeq: R0 = {
    val Tmpl = rule( (Annot ~ OneNLMax).rep ~ Mod.rep ~ (TraitDef | ClsDef | ObjDef) )
    val TopStat = rule( PkgBlock | PkgObj | Import | Tmpl )
    rule( TopStat.rep1(Semis) )
  }
  val TopPkgSeq = rule( (`package` ~ QualId ~ !(WS ~ "{")).rep1(Semis) )
  val CompilationUnit: Rule0 = {
    val Body = rule( TopPkgSeq ~ (Semis ~ TopStatSeq).? | TopStatSeq | "" )
    rule( Semis.? ~ Body ~ Semis.? ~ WL )
  }
}
