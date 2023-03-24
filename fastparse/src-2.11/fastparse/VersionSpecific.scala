package fastparse

trait VersionSpecific{
  object NoWarn{
    class nowarn(msg: String = "")
  }
}