package scalaparser

import fastparse._
trait Types extends Core{

  def TypeExpr: R0
  def ValVarDef: R0
  def FunDef: R0
  private implicit def wspStr(s: String) = R(WL ~ s)(Utils.literalize(s).toString)

  val LocalMod: R0 = R( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  val AccessMod: R0 = {
    val AccessQualifier = R( "[" ~! (`this` | Id) ~ "]" )
    R( (`private` | `protected`) ~ AccessQualifier.? )
  }
  val Dcl: R0 = {
    R( (`val` | `var`) ~! ValVarDef | `def` ~! FunDef | `type` ~! TypeDef )
  }

  val Mod: R0 = R( LocalMod | AccessMod | `override` )

  val Type: R0 = {
    val ExistentialClause = R( `forSome` ~! `{` ~ Dcl.rep1(Semis) ~ `}` )
    val PostfixType = R( InfixType ~ (`=>` ~! Type | ExistentialClause).? )
    val Unbounded = R( `_` | PostfixType )
    R( `=>`.? ~ Unbounded ~ TypeBounds ~ "*".? )
  }

  val InfixType = R( CompoundType ~ (NotNewline ~ Id ~ OneNLMax ~ CompoundType).rep )

  val CompoundType = {
    val Refinement = R( OneNLMax ~ `{` ~ Dcl.rep(Semis) ~ `}` )
    R( AnnotType.rep1(`with` ~! Pass) ~ Refinement.? | Refinement )
  }
  val AnnotType = R(SimpleType ~ (NotNewline ~ (NotNewline ~ Annot).rep1).? )

  val SimpleType: R0 = {
    // Can't `cut` after the opening paren, because we might be trying to parse `()`
    // or `() => T`! only cut after parsing one type
    val BasicType = R( "(" ~ Types.? ~ ")"  | StableId ~ ("." ~ `type`).?)
    R( BasicType ~ (TypeArgs | `#` ~ Id).rep )
  }

  val TypeArgs = R( "[" ~! Types ~ "]" )
  val Types = R( Type ~ ("," ~! Type).rep )

  val FunSig: R0 = {
    val FunArg = R( Annot.rep ~ Id ~ (`:` ~! Type).? ~ (`=` ~! TypeExpr).? )
    val Args = R( FunArg.rep1("," ~! Pass) )
    val FunArgs = R( OneNLMax ~ "(" ~! `implicit`.? ~ Args.? ~ ")" )
    val FunTypeArgs = R( "[" ~ (Annot.rep ~ TypeArg).rep1("," ~! Pass) ~ "]" )
    R( (Id | `this`) ~ FunTypeArgs.? ~ FunArgs.rep )
  }

  val TypeBounds: R0 = R( (`>:` ~ Type).? ~ (`<:` ~ Type).? )
  val TypeArg: R0 = {
    val CtxBounds = R((`<%` ~! Type).rep ~ (`:` ~ Type).rep)
    R((Id | `_`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  val Annot: R0 = R( `@` ~! SimpleType ~  ("(" ~ (Exprs ~ (`:` ~ `_*`).?).? ~ ")").rep )

  val TypeArgList: R0 = {
    val Variant: R0 = R( Annot.rep ~ (WL ~ CharIn("+-")).? ~ TypeArg )
    R( "[" ~ Variant.rep("," ~! Pass) ~ "]" )
  }
  val Exprs: R0 = R( TypeExpr.rep1("," ~! Pass) )
  val TypeDef: R0 = R( Id ~ TypeArgList.? ~ (`=` ~ Type | TypeBounds) )
}
