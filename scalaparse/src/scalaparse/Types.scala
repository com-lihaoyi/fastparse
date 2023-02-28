package scalaparse

import fastparse._
import fastparse._, ScalaWhitespace._
trait Types extends Core{
  def TypeExpr[_: P]: P[Unit]
  def ValVarDef[_: P]: P[Unit]
  def FunDef[_: P]: P[Unit]

  def LocalMod[_: P]: P[Unit] = P( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  def AccessMod[_: P]: P[Unit] = {
    def AccessQualifier = P( "[" ~/ (`this` | Id) ~ "]" )
    P( (`private` | `protected`) ~ AccessQualifier.? )
  }
  def Dcl[_: P]: P[Unit] = {
    P( (`val` | `var`) ~/ ValVarDef | `def` ~/ FunDef | `type` ~/ TypeDef )
  }

  def Mod[_: P]: P[Unit] = P( LocalMod | AccessMod | `override` )

  def ExistentialClause[_: P] = P( `forSome` ~/ `{` ~ Dcl.repX(1, Semis) ~ `}` )
  def PostfixType[_: P] = P( InfixType ~ (`=>` ~/ Type | ExistentialClause).? )
  def Type[_: P]: P[Unit] = P( `=>`.? ~~ PostfixType ~ TypeBounds ~ `*`.? )


  // Can't cut after `*` because we may need to backtrack and settle for
  // the `*`-postfix rather than an infix type
  def InfixType[_: P] = P( CompoundType ~~ (NotNewline ~ (`*` | Id./) ~~ OneNLMax ~ CompoundType).repX )

  def CompoundType[_: P]  = {
    def Refinement = P( OneNLMax ~ `{` ~/ Dcl.repX(sep=Semis) ~ `}` )
    def NamedType = P( (Pass ~ AnnotType).rep(1, `with`./) )
    P( NamedType ~~ Refinement.? | Refinement )
  }
  def NLAnnot[_: P] = P( NotNewline ~ Annot )
  def AnnotType[_: P] = P(SimpleType ~~ NLAnnot.repX )

  def TypeId[_: P] = P( StableId )
  def SimpleType[_: P]: P[Unit] = {
    // Can't `cut` after the opening paren, because we might be trying to parse `()`
    // or `() => T`! only cut after parsing one type
    def TupleType = P( "(" ~/ Type.repTC() ~ ")" )
    def BasicType = P( TupleType | Literals.NoInterp.Literal | TypeId ~ ("." ~ `type`).?  | `_` )
    P( BasicType ~ (TypeArgs | `#` ~/ Id).rep )
  }

  def TypeArgs[_: P] = P( "[" ~/ Type.repTC() ~ "]" )


  def FunSig[_: P]: P[Unit] = {
    def FunArg = P( Annot.rep ~ Id ~ (`:` ~/ Type).? ~ (`=` ~/ TypeExpr).? )
    def Args = P( FunArg.repTC(1) )
    def FunArgs = P( OneNLMax ~ "(" ~/ `implicit`.? ~ Args.? ~ ")" )
    def FunTypeArgs = P( "[" ~/ (Annot.rep ~ TypeArg).repTC(1) ~ "]" )
    P( (Id | `this`) ~ FunTypeArgs.? ~~ FunArgs.rep )
  }

  def TypeBounds[_: P]: P[Unit] = P( (`>:` ~/ Type).? ~ (`<:` ~/ Type).? )
  def TypeArg[_: P]: P[Unit] = {
    def CtxBounds = P((`<%` ~/ Type).rep ~ (`:` ~/ Type).rep)
    P((Id | `_`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  def Annot[_: P]: P[Unit] = P( `@` ~/ SimpleType ~  ("(" ~/ (Exprs ~ (`:` ~/ `_*`).?).? ~ TrailingComma ~ ")").rep )

  def TypeArgList[_: P]: P[Unit] = {
    def Variant: P[Unit] = P( Annot.rep ~ CharIn("+\\-").? ~ TypeArg )
    P( "[" ~/ Variant.repTC(1) ~ "]" )
  }
  def Exprs[_: P]: P[Unit] = P( TypeExpr.rep(1, ",") )
  def TypeDef[_: P]: P[Unit] = P( Id ~ TypeArgList.? ~ (`=` ~/ Type | TypeBounds) )
}
