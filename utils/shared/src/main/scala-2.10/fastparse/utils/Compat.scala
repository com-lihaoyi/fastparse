package fastparse.utils
object Compat{
  type Context = scala.reflect.macros.Context
  def enclosingName(c: Context) = {
    c.asInstanceOf[scala.reflect.macros.runtime.Context]
      .callsiteTyper
      .context
      .owner
      .asInstanceOf[c.Symbol]
  }
}