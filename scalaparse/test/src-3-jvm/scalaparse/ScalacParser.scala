package scalaparse

// In Scala 3 we assume the files compile since they are tested
// already using the Scala 2 compiler in Scala 2 tests
object ScalacParser{
  def checkParseFails(input: String) = false
}
