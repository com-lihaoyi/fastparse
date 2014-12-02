package scalaParser

trait Types extends Core{

  def TypeExpr: R0

  private implicit def wspStr(s: String) = rule( WL ~ str(s) )
  private implicit def wspCh(s: Char) = rule( WL ~ ch(s) )

  def Mod: R0 = rule( LocalMod | AccessMod | `override` )
  def LocalMod: R0 = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  def AccessMod: R0 = {
    def AccessQualifier = rule( '[' ~ (`this` | Id) ~ ']' )
    rule( (`private` | `protected`) ~ AccessQualifier.? )
  }

  def Dcl: R0 = {
    def VarDcl = rule( `var` ~ Ids ~ `:` ~ Type )
    def FunDcl = rule( `def` ~ FunSig ~ (`:` ~ Type).? )
    rule( ValDcl | VarDcl | FunDcl | TypeDcl )
  }

  def Type: R0 = {
    def FunctionArgTypes = rule('(' ~ ParamType.+.sep(',').? ~ ')' )
    def ArrowType = rule( FunctionArgTypes ~ `=>` ~ Type )
    def ExistentialClause = rule( `forSome` ~ `{` ~ (TypeDcl | ValDcl).+.sep(Semis) ~ `}` )
    def PostfixType = rule( InfixType ~ (`=>` ~ Type | ExistentialClause.?) )
    def Unbounded = rule( `_` | ArrowType | PostfixType )
    rule( Unbounded ~ TypeBounds )
  }

  def InfixType = rule( CompoundType ~ (NotNewline ~ Id ~ OneNLMax ~ CompoundType).* )

  def CompoundType = {
    def RefineStat = rule( TypeDef | Dcl  )
    def Refinement = rule( OneNLMax ~ `{` ~ RefineStat.*.sep(Semis) ~ `}` )
    rule( AnnotType.+.sep(`with`) ~ Refinement.? | Refinement )
  }
  def AnnotType = rule(SimpleType ~ (NotNewline ~ (NotNewline ~ Annot).+).? )

  def SimpleType: R0 = {
    def BasicType = rule( '(' ~ Types ~ ')'  | StableId ~ '.' ~ `type` | StableId )
    rule( BasicType ~ (TypeArgs | `#` ~ Id).* )
  }

  def TypeArgs = rule( '[' ~ Types ~ "]" )
  def Types = rule( Type.+.sep(',') )

  def ValDcl: R0 = rule( `val` ~ Ids ~ `:` ~ Type )
  def TypeDcl: R0 = rule( `type` ~ Id ~ TypeArgList.? ~ TypeBounds )

  def FunSig: R0 = {
    def FunTypeArgs = rule( '[' ~ (Annot.* ~ TypeArg).+.sep(',') ~ ']' )
    def FunAllArgs = rule( FunArgs.* ~ (OneNLMax ~ '(' ~ `implicit` ~ Args ~ ')').? )
    def FunArgs = rule( OneNLMax ~ '(' ~ Args.? ~ ')' )
    def FunArg = rule( Annot.* ~ Id ~ (`:` ~ ParamType).? ~ (`=` ~ TypeExpr).? )
    def Args = rule( FunArg.+.sep(',') )
    rule( (Id | `this`) ~ FunTypeArgs.? ~ FunAllArgs )
  }
  def ParamType = rule( `=>` ~ Type | Type ~ "*" | Type )

  def TypeBounds: R0 = rule( (`>:` ~ Type).? ~ (`<:` ~ Type).? )
  def TypeArg: R0 = {
    def CtxBounds = rule((`<%` ~ Type).* ~ (`:` ~ Type).*)
    rule((Id | `_`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  def Annot: R0 = rule( `@` ~ SimpleType ~  ('(' ~ (Exprs ~ (`:` ~ `_*`).?).? ~ ")").*  )

  def TypeArgList: R0 = {
    def Variant: R0 = rule( Annot.* ~ (WL ~ anyOf("+-")).? ~ TypeArg )
    rule( '[' ~ Variant.*.sep(',') ~ ']' )
  }
  def Exprs: R0 = rule( TypeExpr.+.sep(',') )
  def TypeDef: R0 = rule( `type` ~ Id ~ TypeArgList.? ~ `=` ~ Type )
}
