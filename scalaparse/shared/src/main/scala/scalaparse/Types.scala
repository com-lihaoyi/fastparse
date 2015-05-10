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

  val Type: P0 = {
    val ExistentialClause = P( `forSome` ~! `{` ~ Dcl.rep1(Semis) ~ `}` )
    val PostfixType = P( InfixType ~ (`=>` ~! Type | ExistentialClause).? )
    val Unbounded = P( `_` | PostfixType )
    P( `=>`.? ~ Unbounded ~ TypeBounds ~ "*".? )
  }

  val InfixType = P( CompoundType ~ (NotNewline ~ Id ~ OneNLMax ~ CompoundType).rep )

  val CompoundType = {
    val Refinement = P( OneNLMax ~ `{` ~ Dcl.rep(Semis) ~ `}` )
    P( AnnotType.rep1(`with` ~! Pass) ~ Refinement.? | Refinement )
  }
  val AnnotType = P(SimpleType ~ (NotNewline ~ (NotNewline ~ Annot).rep1).? )

  val SimpleType: P0 = {
    // Can't `cut` after the opening paren, because we might be trying to parse `()`
    // or `() => T`! only cut after parsing one type
    val BasicType = P( "(" ~ Types.? ~ ")"  | StableId ~ ("." ~ `type`).?)
    P( BasicType ~ (TypeArgs | `#` ~! Id).rep )
  }

  val TypeArgs = P( "[" ~! Types ~ "]" )
  val Types = P( Type ~ ("," ~! Type).rep )

  val FunSig: P0 = {
    val FunArg = P( Annot.rep ~ Id ~ (`:` ~! Type).? ~ (`=` ~! TypeExpr).? )
    val Args = P( FunArg.rep1("," ~! Pass) )
    val FunArgs = P( OneNLMax ~ "(" ~! `implicit`.? ~ Args.? ~ ")" )
    val FunTypeArgs = P( "[" ~! (Annot.rep ~ TypeArg).rep1("," ~! Pass) ~ "]" )
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
    P( "[" ~! Variant.rep1("," ~! Pass) ~ "]" )
  }
  val Exprs: P0 = P( TypeExpr.rep1("," ~! Pass) )
  val TypeDef: P0 = P( Id ~ TypeArgList.? ~ (`=` ~! Type | TypeBounds) )
}
