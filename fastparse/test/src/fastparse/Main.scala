package test.fastparse

object Main {
  def main(args: Array[String]): Unit = {
    import fastparse._, NoWhitespace._
    def iam[$: P] = P( "i am" )
    def hello[$: P] = P( "hello" )
    def combined[$: P] = P( (iam | hello).? ~ ("cow" | "world") )
    val Parsed.Failure(_, _, extra) = parse("lol", combined(_))
    println(extra.trace().longAggregateMsg)
  }
}
