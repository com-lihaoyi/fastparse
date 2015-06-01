package scalaparse

import fastparse._
trait Types extends Core{

  def TypeExpr: P0
  def ValVarDef: P0
  def FunDef: P0
  private implicit def wspStr(s: String) = P(WL ~ s)(Utils.literalize(s).toString)

  val LocalMod: P0 = P( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  val AccessMod: P0 = {
    val AccessQualifier = P( "[" ~! (`this` | Id) ~ "]" )
    P( (`private` | `protected`) ~ AccessQualifier.? )
  }
  val Dcl: P0 = {
    P( (`val` | `var`) ~! ValVarDef | `def` ~! FunDef | `type` ~! TypeDef )
  }

  val Mod: P0 = P( LocalMod | AccessMod | `override` )

  val ExistentialClause = P( `forSome` ~! `{` ~ Dcl.rep(1, Semis) ~ `}` )
  val PostfixType = P( InfixType ~ (`=>` ~! Type | ExistentialClause).? )
  val Type: P0 = {
    val Unbounded = P( `_` | PostfixType )
    P( `=>`.? ~ Unbounded ~ TypeBounds ~ "*".? )
  }

  val InfixType = P( CompoundType ~ (NotNewline ~ Id ~ OneNLMax ~ CompoundType).rep )

  val CompoundType = {
    val Refinement = P( OneNLMax ~ `{` ~ Dcl.rep(sep=Semis) ~ `}` )
    P( AnnotType.rep(1, `with` ~!) ~ Refinement.? | Refinement )
  }
  val AnnotType = P(SimpleType ~ (NotNewline ~ (NotNewline ~ Annot).rep(1)).? )

  val SimpleType: P0 = {
    // Can't `cut` after the opening paren, because we might be trying to parse `()`
    // or `() => T`! only cut after parsing one type
    val BasicType = P( "(" ~ Type.rep(sep="," ~!, end = ")") | StableId ~ ("." ~ `type`).?)
    P( BasicType ~ (TypeArgs | `#` ~! Id).rep )
  }

  val TypeArgs = P( "[" ~! Type.rep(sep="," ~!, end = "]") )


  val FunSig: P0 = {
    val FunArg = P( Annot.rep ~ Id ~ (`:` ~! Type).? ~ (`=` ~! TypeExpr).? )
    val Args = P( FunArg.rep(1, "," ~!) )
    val FunArgs = P( OneNLMax ~ "(" ~! `implicit`.? ~ Args.? ~ ")" )
    val FunTypeArgs = P( "[" ~! (Annot.rep ~ TypeArg).rep(1, "," ~!) ~ "]" )
    P( (Id | `this`) ~ FunTypeArgs.? ~ FunArgs.rep )
  }

  val TypeBounds: P0 = P( (`>:` ~! Type).? ~ (`<:` ~! Type).? )
  val TypeArg: P0 = {
    val CtxBounds = P((`<%` ~! Type).rep ~ (`:` ~! Type).rep)
    P((Id | `_`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  val Annot: P0 = P( `@` ~! SimpleType ~  ("(" ~! (Exprs ~ (`:` ~! `_*`).?).? ~ ")").rep )

  val TypeArgList: P0 = {
    val Variant: P0 = P( Annot.rep ~ (WL ~ CharIn("+-")).? ~ TypeArg )
    P( "[" ~! Variant.rep(1, "," ~! ) ~ "]" )
  }
  val Exprs: P0 = P( TypeExpr.rep(1, "," ~! Pass) )
  val TypeDef: P0 = P( Id ~ TypeArgList.? ~ (`=` ~! Type | TypeBounds) )
}
