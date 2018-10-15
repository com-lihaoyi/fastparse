package test.fastparse

object Main {
  def main(args: Array[String]): Unit = {
    import fastparse._, NoWhitespace._
    def iam[_: P] = P( "i am" )
    def hello[_: P] = P( "hello" )
    def combined[_: P] = P( (iam | hello).? ~ ("cow" | "world") )
    val Parsed.Failure(_, _, extra) = parse("lol", combined(_))
    println(extra.traced.trace)
  }
}
