package scalaparse

import fastparse._
trait Types extends Core{
  private implicit def parserApi[T, V](p0: T)
                                      (implicit c: T => P[V]) =
    new ParserApiImpl2[V](p0, WL)
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
    P( WL ~ ((`val` | `var`) ~! ValVarDef | `def` ~! FunDef | `type` ~! TypeDef) )
  }

  val Mod: P0 = P( LocalMod | AccessMod | `override` )

  val ExistentialClause = P( `forSome` ~! `{` ~ Dcl.rep(1, Semis) ~ `}` )
  val PostfixType = P( InfixType ~ (`=>` ~! Type | ExistentialClause).? )
  val Type: P0 = P( `=>`.? ~~ PostfixType ~ TypeBounds ~ `*`.? )


  // Can't cut after `Id` because it may be a `*`, in which case
  // we may need to backtrack and settle for the `*`-postfix rather than
  // an infix type
  val InfixType = P( CompoundType ~~ (NotNewline ~ Id ~~ OneNLMax ~ CompoundType).rep )

  val CompoundType = {
    val Refinement = P( OneNLMax ~ `{` ~ Dcl.rep(sep=Semis) ~ `}` )
    val NamedRefinement = P( WL ~ AnnotType.rep(1, `with` ~!) ~~ Refinement.? )
    P( NamedRefinement | Refinement )
  }
  val AnnotType = P(SimpleType ~~ (NotNewline ~~ (NotNewline ~ Annot).rep(1)).? )

  val SimpleType: P0 = {
    // Can't `cut` after the opening paren, because we might be trying to parse `()`
    // or `() => T`! only cut after parsing one type
    val TupleType = P( "(" ~ Type.rep(sep="," ~!, end = ")") )
    val BasicType = P( TupleType | StableId ~ ("." ~ `type`).? | `_` )
    P( BasicType ~ (WL ~ (TypeArgs | `#` ~! Id)).rep )
  }

  val TypeArgs = P( "[" ~! Type.rep(sep="," ~!, end = "]") )


  val FunSig: P0 = {
    val FunArg = P( Annot.rep ~ Id ~ (`:` ~! Type).? ~ (`=` ~! TypeExpr).? )
    val Args = P( FunArg.rep(1, "," ~!) )
    val FunArgs = P( OneNLMax ~ "(" ~! (WL ~ `implicit`).? ~ Args.? ~ ")" )
    val FunTypeArgs = P( "[" ~! (Annot.rep ~ TypeArg).rep(1, "," ~!) ~ "]" )
    P( (Id | `this`) ~ (WL ~ FunTypeArgs).? ~~ FunArgs.rep )
  }

  val TypeBounds: P0 = P( (WL ~ `>:` ~! Type).? ~ (`<:` ~! Type).? )
  val TypeArg: P0 = {
    val CtxBounds = P((`<%` ~! Type).rep ~ (`:` ~! Type).rep)
    P((Id | `_`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  val Annot: P0 = P( `@` ~! SimpleType ~  ("(" ~! (Exprs ~ (`:` ~! `_*`).?).? ~ ")").rep )

  val TypeArgList: P0 = {
    val Variant: P0 = P( Annot.rep ~ CharIn("+-").? ~ TypeArg )
    P( "[" ~! Variant.rep(1, "," ~! ) ~ "]" )
  }
  val Exprs: P0 = P( TypeExpr.rep(1, "," ~! Pass) )
  val TypeDef: P0 = P( Id ~ TypeArgList.? ~ (`=` ~! Type | TypeBounds) )
}
