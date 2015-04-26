package parsing

object Parsing {
  sealed trait Parser[T]
  case class Literal(s: String) extends Parser[String]
  case class Regex(re: scala.util.matching.Regex) extends Parser[String]
  implicit val stringToLiteral = Literal
  implicit val regexToRegex = Regex
  def rule[T](p: Parser[T]): Parser[T] = p
}
