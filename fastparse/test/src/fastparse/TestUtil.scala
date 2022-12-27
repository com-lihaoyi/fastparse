package test.fastparse

object TestUtil {
  def skipScala3(code: => Unit): Unit = {
    if(BuildInfo.scalaVersion.startsWith("2.")) code
  }
}
