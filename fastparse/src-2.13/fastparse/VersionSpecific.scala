package fastparse

trait VersionSpecific{
  object NoWarn{
    type nowarn = scala.annotation.nowarn
  }
}