package scalaparse

import fastparse._
import fastparse._, ScalaWhitespace._
trait Types extends Core{
  def TypeExpr[$: P]: P[Unit]
  def ValVarDef[$: P]: P[Unit]
  def FunDef[$: P]: P[Unit]

  def LocalMod[$: P]: P[Unit] = P( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  def AccessMod[$: P]: P[Unit] = {
    def AccessQualifier = P( "[" ~/ (`this` | Id) ~ "]" )
    P( (`private` | `protected`) ~ AccessQualifier.? )
  }
  def Dcl[$: P]: P[Unit] = {
    P( (`val` | `var`) ~/ ValVarDef | `def` ~/ FunDef | `type` ~/ TypeDef )
  }

  def Mod[$: P]: P[Unit] = P( LocalMod | AccessMod | `override` )

  def ExistentialClause[$: P] = P( `forSome` ~/ `{` ~ Dcl.repX(1, Semis) ~ `}` )
  def PostfixType[$: P] = P( InfixType ~ (`=>` ~/ Type | ExistentialClause).? )
  def Type[$: P]: P[Unit] = P( `=>`.? ~~ PostfixType ~ TypeBounds ~ `*`.? )


  // Can't cut after `*` because we may need to backtrack and settle for
  // the `*`-postfix rather than an infix type
  def InfixType[$: P] = P( CompoundType ~~ (NotNewline ~ (`*` | Id./) ~~ OneNLMax ~ CompoundType).repX )

  def CompoundType[$: P]  = {
    def Refinement = P( OneNLMax ~ `{` ~/ Dcl.repX(sep=Semis) ~ `}` )
    def NamedType = P( (Pass ~ AnnotType).rep(1, `with`./) )
    P( NamedType ~~ Refinement.? | Refinement )
  }
  def NLAnnot[$: P] = P( NotNewline ~ Annot )
  def AnnotType[$: P] = P(SimpleType ~~ NLAnnot.repX )

  def TypeId[$: P] = P( StableId )
  def SimpleType[$: P]: P[Unit] = {
    // Can't `cut` after the opening paren, because we might be trying to parse `()`
    // or `() => T`! only cut after parsing one type
    def TupleType = P( "(" ~/ Type.repTC() ~ ")" )
    def BasicType = P( TupleType | Literals.NoInterp.Literal | TypeId ~ ("." ~ `type`).?  | Underscore )
    P( BasicType ~ (TypeArgs | `#` ~/ Id).rep )
  }

  def TypeArgs[$: P] = P( "[" ~/ Type.repTC() ~ "]" )


  def FunSig[$: P]: P[Unit] = {
    def FunArg = P( Annot.rep ~ Id ~ (`:` ~/ Type).? ~ (`=` ~/ TypeExpr).? )
    def Args = P( FunArg.repTC(1) )
    def FunArgs = P( OneNLMax ~ "(" ~/ `implicit`.? ~ Args.? ~ ")" )
    def FunTypeArgs = P( "[" ~/ (Annot.rep ~ TypeArg).repTC(1) ~ "]" )
    P( (Id | `this`) ~ FunTypeArgs.? ~~ FunArgs.rep )
  }

  def TypeBounds[$: P]: P[Unit] = P( (`>:` ~/ Type).? ~ (`<:` ~/ Type).? )
  def TypeArg[$: P]: P[Unit] = {
    def CtxBounds = P((`<%` ~/ Type).rep ~ (`:` ~/ Type).rep)
    P((Id | Underscore) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  def Annot[$: P]: P[Unit] = P( `@` ~/ SimpleType ~  ("(" ~/ (Exprs ~ (`:` ~/ `Underscore*`).?).? ~ TrailingComma ~ ")").rep )

  def TypeArgList[$: P]: P[Unit] = {
    def Variant: P[Unit] = P( Annot.rep ~ CharIn("+\\-").? ~ TypeArg )
    P( "[" ~/ Variant.repTC(1) ~ "]" )
  }
  def Exprs[$: P]: P[Unit] = P( TypeExpr.rep(1, ",") )
  def TypeDef[$: P]: P[Unit] = P( Id ~ TypeArgList.? ~ (`=` ~/ Type | TypeBounds) )
}

