package fastparse.utils

object Compat{
  type Context = scala.reflect.macros.blackbox.Context
  def enclosingName(c: Context) = {
    c.internal.enclosingOwner
  }
}