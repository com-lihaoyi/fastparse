package parsing

object Parsing {
  sealed trait Res[+T]
  object Res{
    case class Success[T](t: T, index: Int) extends Res[T]
    case class Failure(index: Int) extends Res[Nothing]
  }
  sealed trait Parser[T]{
    def parse(s: String, index: Int): Res[T]
  }
  object Parser{
//    case class Repeat[T](p: Parser[T]) extends Parser[Vector[T]]{
//      def parse
//    }
//    case class Either[T, V1 <: T, V2 <: T](p1: Parser[V1], p2: Parser[V2]) extends Parser[T]
    case class Literal(s: String) extends Parser[String]{
      def parse(input: String, index: Int) = {
        println("LITERAL |" + s + "|" + input + "|" + index + "|" + input.startsWith(s, index))

        if (input.startsWith(s, index)) Res.Success(s, index + s.length)
        else Res.Failure(index)
      }
    }
//    case class Regex(re: scala.util.matching.Regex) extends Parser[String]
  }
  implicit val stringToLiteral = Parser.Literal
//  implicit val regexToRegex = Parser.Regex
  def rule[T](p: Parser[T]): Parser[T] = p
  def main(args: Array[String]): Unit = {
    println(23)
  }
}

