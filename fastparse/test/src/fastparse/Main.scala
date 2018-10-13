package test.fastparse

object Main {
  def main(args: Array[String]): Unit = {
    import fastparse._, NoWhitespace._
    def left[_: P] = P( "hello" ~ "world" )
    def right[_: P] = P( "i" ~ "am" ~ "cow" )
    def combined[_: P] = P( left | right )
    val Parsed.Failure(_, _, extra) = parse("hellocow").read(combined(_))
    println(extra.traced.trace)
  }
}
