package test.fastparse

object Main {
  def main(args: Array[String]): Unit = {
    import fastparse._, NoWhitespace._
    def iam[_p: P] = P( "i am" )
    def hello[_p: P] = P( "hello" )
    def combined[_p: P] = P( (iam | hello).? ~ ("cow" | "world") )
    val Parsed.Failure(_, _, extra) = parse("lol", combined(_))
    println(extra.trace().longAggregateMsg)
  }
}
