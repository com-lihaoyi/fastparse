package scalaParser
import parsing._
trait Types extends Core{

  def TypeExpr: R0
  def ValDef: R0
  def VarDef: R0
  def DefDef: R0
  private implicit def wspStr(s: String) = WL ~ s

  val Mod: R0 = R( LocalMod | AccessMod | `override` )
  val LocalMod: R0 = R( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  val AccessMod: R0 = {
    val AccessQualifier = R( "[" ~ (`this` | Id) ~ "]" )
    R( (`private` | `protected`) ~ AccessQualifier.? )
  }

  val ValDcl = R( Ids ~ `:` ~ Type )
  val VarDcl = R( Ids ~ `:` ~ Type )
  val FunDcl = R( FunSig ~ (`:` ~ Type).? )
  val Dcl: R0 = {
    R( (`val` ~ ValDcl) | (`var` ~ VarDcl) | (`def` ~ FunDcl) | (`type` ~ TypeDcl) )
  }

  val Type: R0 = {
    val FunctionArgTypes = R("(" ~ ParamType.rep1(",").? ~ ")" )
    val ArrowType = R( FunctionArgTypes ~ `=>` ~ Type )
    val ExistentialClause = R( `forSome` ~ `{` ~ Dcl.rep1(Semis) ~ `}` )
    val PostfixType = R( InfixType ~ (`=>` ~ Type | ExistentialClause.?) )
    val Unbounded = R( `_` | ArrowType | PostfixType )
    R( Unbounded ~ TypeBounds )
  }

  val InfixType = R( CompoundType ~ (NotNewline ~ Id ~ OneNLMax ~ CompoundType).rep )

  val CompoundType = {
    val RefineStat = R( (`type` ~ TypeDef) | Dcl  )
    val Refinement = R( OneNLMax ~ `{` ~ RefineStat.rep(Semis) ~ `}` )
    R( AnnotType.rep1(`with`) ~ Refinement.? | Refinement )
  }
  val AnnotType = R(SimpleType ~ (NotNewline ~ (NotNewline ~ Annot).rep1).? )

  val SimpleType: R0 = {
    val BasicType = R( "(" ~ Types ~ ")"  | StableId ~ "." ~ `type` | StableId )
    R( BasicType ~ (TypeArgs | `#` ~ Id).rep )
  }

  val TypeArgs = R( "[" ~ Types ~ "]" )
  val Types = R( Type.rep1(",") )

  val TypeDcl: R0 = R( Id ~ TypeArgList.? ~ TypeBounds )

  val FunSig: R0 = {
    val FunArg = R( Annot.rep ~ Id ~ (`:` ~ ParamType).? ~ (`=` ~ TypeExpr).? )
    val Args = R( FunArg.rep1(",") )
    val FunArgs = R( OneNLMax ~ "(" ~ Args.? ~ ")" )
    val FunAllArgs = R( FunArgs.rep ~ (OneNLMax ~ "(" ~ `implicit` ~ Args ~ ")").? )
    val FunTypeArgs = R( "[" ~ (Annot.rep ~ TypeArg).rep1(",") ~ "]" )
    R( (Id | `this`) ~ FunTypeArgs.? ~ FunAllArgs )
  }
  val ParamType = R( `=>` ~ Type | Type ~ "*" | Type )

  val TypeBounds: R0 = R( (`>:` ~ Type).? ~ (`<:` ~ Type).? )
  val TypeArg: R0 = {
    val CtxBounds = R((`<%` ~ Type).rep ~ (`:` ~ Type).rep)
    R((Id | `_`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  val Annot: R0 = R( `@` ~ SimpleType ~  ("(" ~ (Exprs ~ (`:` ~ `_*`).?).? ~ ")").rep)

  val TypeArgList: R0 = {
    val Variant: R0 = R( Annot.rep ~ (WL ~ CharIn("+-")).? ~ TypeArg )
    R( "[" ~ Variant.rep(",") ~ "]" )
  }
  val Exprs: R0 = R( TypeExpr.rep1(",") )
  val TypeDef: R0 = R( Id ~ TypeArgList.? ~ `=` ~ Type )
}
