package scalaParser

import macros.Macros._

trait Types extends Core{

  def Expr: R0

  private implicit def wspStr(s: String) = rule( WL ~ str(s) )
  private implicit def wspCh(s: Char) = rule( WL ~ ch(s) )

  import KeyWordOperators._
  import KeyWordOperators.`_`

  def Mod: R0 = rule( LocalMod | AccessMod | `override` )
  def LocalMod: R0 = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  def AccessMod: R0 = {
    def AccessQualifier = rule( '[' ~ (`this` | Id) ~ ']' )
    rule( (`private` | `protected`) ~ opt(AccessQualifier) )
  }

  def Dcl: R0 = {
    def VarDcl = rule( `var` ~ Ids ~ `:` ~ Type )
    def FunDcl = rule( `def` ~ FunSig ~ opt(`:` ~ Type) )
    rule( ValDcl | VarDcl | FunDcl | TypeDcl )
  }

  def Type: R0 = {
    def FunctionArgTypes = rule('(' ~ opt(rep1Sep(ParamType, ',')) ~ ')' )
    def ArrowType = rule( FunctionArgTypes ~ `=>` ~ Type )
    def ExistentialClause = rule( `forSome` ~ `{` ~ rep1Sep(TypeDcl | ValDcl, Semis) ~ `}` )
    def PostfixType = rule( InfixType ~ (`=>` ~ Type | opt(ExistentialClause)) )
    def Unbounded = rule( `_` | ArrowType | PostfixType )
    rule( Unbounded ~ TypeBounds )
  }

  def InfixType = rule( CompoundType ~ rep(NotNewline ~ Id ~ OneNLMax ~ CompoundType) )

  def CompoundType = {
    def RefineStat = rule( TypeDef | Dcl  )
    def Refinement = rule( OneNLMax ~ `{` ~ repSep(RefineStat, Semis) ~ `}` )
    rule( rep1Sep(AnnotType, `with`) ~ opt(Refinement) | Refinement )
  }
  def AnnotType = rule(SimpleType ~ opt(NotNewline ~ rep1(NotNewline ~ Annot)) )

  def SimpleType: R0 = {
    def BasicType = rule( '(' ~ Types ~ ')'  | StableId ~ '.' ~ `type` | StableId )
    rule( BasicType ~ rep(TypeArgs | `#` ~ Id) )
  }

  def TypeArgs = rule( '[' ~ Types ~ "]" )
  def Types = rule( rep1Sep(Type, ',') )

  def ValDcl: R0 = rule( `val` ~ Ids ~ `:` ~ Type )
  def TypeDcl: R0 = rule( `type` ~ Id ~ opt(TypeArgList) ~ TypeBounds )

  def FunSig: R0 = {
    def FunTypeArgs = rule( '[' ~ rep1Sep(rep(Annot) ~ TypeArg, ',') ~ ']' )
    def FunAllArgs = rule( rep(FunArgs) ~ opt(OneNLMax ~ '(' ~ `implicit` ~ Args ~ ')') )
    def FunArgs = rule( OneNLMax ~ '(' ~ opt(Args) ~ ')' )
    def FunArg = rule( rep(Annot) ~ Id ~ opt(`:` ~ ParamType) ~ opt(`=` ~ Expr) )
    def Args = rule( rep1Sep(FunArg, ',') )
    rule( (Id | `this`) ~ opt(FunTypeArgs) ~ FunAllArgs )
  }
  def ParamType = rule( `=>` ~ Type | Type ~ "*" | Type )

  def TypeBounds: R0 = rule( opt(`>:` ~ Type) ~ opt(`<:` ~ Type) )
  def TypeArg: R0 = {
    def CtxBounds = rule(rep(`<%` ~ Type) ~ rep(`:` ~ Type))
    rule((Id | `_`) ~ opt(TypeArgList) ~ TypeBounds ~ CtxBounds)
  }

  def Annot: R0 = rule( `@` ~ SimpleType ~  rep('(' ~ opt(Exprs ~ opt(`:` ~ `_*`)) ~ ")")  )

  def TypeArgList: R0 = {
    def Variant: R0 = rule( rep(Annot) ~ opt(WL ~ anyOf("+-")) ~ TypeArg )
    rule( '[' ~ rep1Sep(Variant, ',') ~ ']' )
  }
  def Exprs: R0 = rule( rep1Sep(Expr, ',') )
  def TypeDef: R0 = rule( `type` ~ Id ~ opt(TypeArgList) ~ `=` ~ Type )
}
