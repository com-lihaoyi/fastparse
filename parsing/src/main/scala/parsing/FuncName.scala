package parsing

import scala.reflect.macros.blackbox.Context
import acyclic.file
import scala.language.experimental.macros

/**
 * Type, which when summoned implicitly, provides the
 * name of the nearest enclosing method for your perusal
 */
case class FuncName(name: String)
object FuncName{
  implicit def strToFuncName(s: String) = FuncName(s)

  def impl(c: Context): c.Expr[FuncName] = {
    import c.universe._
    val sym = c.internal.enclosingOwner
    val simpleName = sym.name.decodedName.toString.trim

    val name = q"$simpleName"

    c.Expr[FuncName](q"parsing.FuncName($name)")
  }
}
