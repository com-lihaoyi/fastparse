package scalaParser
import parsing._
trait Types extends Core{

  def TypeExpr: R0
  def ValDef: R0
  def VarDef: R0
  def DefDef: R0
  private implicit def wspStr(s: String) = WL ~ s

  val Mod: R0 = rule( LocalMod | AccessMod | `override` )
  val LocalMod: R0 = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  val AccessMod: R0 = {
    val AccessQualifier = rule( "[" ~ (`this` | Id) ~ "]" )
    rule( (`private` | `protected`) ~ AccessQualifier.? )
  }

  val Dcl: R0 = {
    val ValDcl = rule( ValDef | `val` ~ Ids ~ `:` ~ Type )
    val VarDcl = rule( VarDef | `var` ~ Ids ~ `:` ~ Type )
    val FunDcl = rule( DefDef | `def` ~ FunSig ~ (`:` ~ Type).? )
    rule( ValDcl | VarDcl | FunDcl | TypeDcl )
  }

  val Type: R0 = {
    val FunctionArgTypes = rule("(" ~ ParamType.rep1(",").? ~ ")" )
    val ArrowType = rule( FunctionArgTypes ~ `=>` ~ Type )
    val ExistentialClause = rule( `forSome` ~ `{` ~ Dcl.rep1(Semis) ~ `}` )
    val PostfixType = rule( InfixType ~ (`=>` ~ Type | ExistentialClause.?) )
    val Unbounded = rule( `_` | ArrowType | PostfixType )
    rule( Unbounded ~ TypeBounds )
  }

  val InfixType = rule( CompoundType ~ (NotNewline ~ Id ~ OneNLMax ~ CompoundType).rep )

  val CompoundType = {
    val RefineStat = rule( TypeDef | Dcl  )
    val Refinement = rule( OneNLMax ~ `{` ~ RefineStat.rep(Semis) ~ `}` )
    rule( AnnotType.rep1(`with`) ~ Refinement.? | Refinement )
  }
  val AnnotType = rule(SimpleType ~ (NotNewline ~ (NotNewline ~ Annot).rep1).? )

  val SimpleType: R0 = {
    val BasicType = rule( "(" ~ Types ~ ")"  | StableId ~ "." ~ `type` | StableId )
    rule( BasicType ~ (TypeArgs | `#` ~ Id).rep )
  }

  val TypeArgs = rule( "[" ~ Types ~ "]" )
  val Types = rule( Type.rep1(",") )

  val TypeDcl: R0 = rule( `type` ~ Id ~ TypeArgList.? ~ TypeBounds )

  val FunSig: R0 = {
    val FunArg = rule( Annot.rep ~ Id ~ (`:` ~ ParamType).? ~ (`=` ~ TypeExpr).? )
    val Args = rule( FunArg.rep1(",") )
    val FunArgs = rule( OneNLMax ~ "(" ~ Args.? ~ ")" )
    val FunAllArgs = rule( FunArgs.rep ~ (OneNLMax ~ "(" ~ `implicit` ~ Args ~ ")").? )
    val FunTypeArgs = rule( "[" ~ (Annot.rep ~ TypeArg).rep1(",") ~ "]" )
    rule( (Id | `this`) ~ FunTypeArgs.? ~ FunAllArgs )
  }
  val ParamType = rule( `=>` ~ Type | Type ~ "*" | Type )

  val TypeBounds: R0 = rule( (`>:` ~ Type).? ~ (`<:` ~ Type).? )
  val TypeArg: R0 = {
    val CtxBounds = rule((`<%` ~ Type).rep ~ (`:` ~ Type).rep)
    rule((Id | `_`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  val Annot: R0 = rule( `@` ~ SimpleType ~  ("(" ~ (Exprs ~ (`:` ~ `_*`).?).? ~ ")").rep)

  val TypeArgList: R0 = {
    val Variant: R0 = rule( Annot.rep ~ (WL ~ CharSets("+-")).? ~ TypeArg )
    rule( "[" ~! Variant.rep(",") ~ "]" )
  }
  val Exprs: R0 = rule( TypeExpr.rep1(",") )
  val TypeDef: R0 = rule( `type` ~ Id ~ TypeArgList.? ~ `=` ~ Type )
}
