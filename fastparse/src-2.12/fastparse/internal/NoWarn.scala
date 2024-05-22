package fastparse.internal

object NoWarn {
  @deprecated("Use scala.annotation.nowarn instead", "3.1.1")
  class nowarn(msg: String = "")
}
