package scalaparse

import fastparse._
import fastparse._, ScalaWhitespace._
trait Types extends Core{
  def TypeExpr[_p: P]: P[Unit]
  def ValVarDef[_p: P]: P[Unit]
  def FunDef[_p: P]: P[Unit]

  def LocalMod[_p: P]: P[Unit] = P( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  def AccessMod[_p: P]: P[Unit] = {
    def AccessQualifier = P( "[" ~/ (`this` | Id) ~ "]" )
    P( (`private` | `protected`) ~ AccessQualifier.? )
  }
  def Dcl[_p: P]: P[Unit] = {
    P( (`val` | `var`) ~/ ValVarDef | `def` ~/ FunDef | `type` ~/ TypeDef )
  }

  def Mod[_p: P]: P[Unit] = P( LocalMod | AccessMod | `override` )

  def ExistentialClause[_p: P] = P( `forSome` ~/ `{` ~ Dcl.repX(1, Semis) ~ `}` )
  def PostfixType[_p: P] = P( InfixType ~ (`=>` ~/ Type | ExistentialClause).? )
  def Type[_p: P]: P[Unit] = P( `=>`.? ~~ PostfixType ~ TypeBounds ~ `*`.? )


  // Can't cut after `*` because we may need to backtrack and settle for
  // the `*`-postfix rather than an infix type
  def InfixType[_p: P] = P( CompoundType ~~ (NotNewline ~ (`*` | Id./) ~~ OneNLMax ~ CompoundType).repX )

  def CompoundType[_p: P] = {
    def Refinement = P( OneNLMax ~ `{` ~/ Dcl.repX(sep=Semis) ~ `}` )
    def NamedType = P( (/* Pass ~ */AnnotType).rep(1, `with`./) )
    //                     ^^^^^^ This doesn't compile on Scala 3
    P( NamedType ~~ Refinement.? | Refinement )
  }
  def NLAnnot[_p: P] = P( NotNewline ~ Annot )
  def AnnotType[_p: P] = P(SimpleType ~~ NLAnnot.repX )

  def TypeId[_p: P] = P( StableId )
  def SimpleType[_p: P]: P[Unit] = {
    // Can't `cut` after the opening paren, because we might be trying to parse `()`
    // or `() => T`! only cut after parsing one type
    def TupleType = P( "(" ~/ Type.repTC() ~ ")" )
    def BasicType = P( TupleType | Literals.NoInterp.Literal | TypeId ~ ("." ~ `type`).?  | `__` )
    P( BasicType ~ (TypeArgs | `#` ~/ Id).rep )
  }

  def TypeArgs[_p: P] = P( "[" ~/ Type.repTC() ~ "]" )


  def FunSig[_p: P]: P[Unit] = {
    def FunArg = P( Annot.rep ~ Id ~ (`:` ~/ Type).? ~ (`=` ~/ TypeExpr).? )
    def Args = P( FunArg.repTC(1) )
    def FunArgs = P( OneNLMax ~ "(" ~/ `implicit`.? ~ Args.? ~ ")" )
    def FunTypeArgs = P( "[" ~/ (Annot.rep ~ TypeArg).repTC(1) ~ "]" )
    P( (Id | `this`) ~ FunTypeArgs.? ~~ FunArgs.rep )
  }

  def TypeBounds[_p: P]: P[Unit] = P( (`>:` ~/ Type).? ~ (`<:` ~/ Type).? )
  def TypeArg[_p: P]: P[Unit] = {
    def CtxBounds = P((`<%` ~/ Type).rep ~ (`:` ~/ Type).rep)
    P((Id | `__`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  def Annot[_p: P]: P[Unit] = P( `@` ~/ SimpleType ~  ("(" ~/ (Exprs ~ (`:` ~/ `_*`).?).? ~ TrailingComma ~ ")").rep )

  def TypeArgList[_p: P]: P[Unit] = {
    def Variant: P[Unit] = P( Annot.rep ~ CharIn("+\\-").? ~ TypeArg )
    P( "[" ~/ Variant.repTC(1) ~ "]" )
  }
  def Exprs[_p: P]: P[Unit] = P( TypeExpr.rep(1, ",") )
  def TypeDef[_p: P]: P[Unit] = P( Id ~ TypeArgList.? ~ (`=` ~/ Type | TypeBounds) )
}
