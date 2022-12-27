package scalaparse.syntax
import fastparse._
import fastparse._
import NoWhitespace._
import Basic._
import CharPredicates._
object Identifiers{

  case class NamedFunction(f: Char => Boolean)
                          (implicit name: sourcecode.Name) extends (Char => Boolean){
    def apply(t: Char) = f(t)
    override def toString() = name.value

  }
  val OpCharNotSlash = NamedFunction(x => isOpChar(x) && x != '/')
  val NotBackTick = NamedFunction(_ != '`')

  def Operator[_p: P] = P(
    !SymbolicKeywords ~ (!StringIn("/*", "//") ~ (CharsWhile(OpCharNotSlash) | "/")).rep(1)
  ).opaque("operator")

  def VarId[_p: P] = P( VarId0(true) ).opaque("var-id")

  def VarId0[_p: P](dollar: Boolean) = P( !Keywords ~ Lower ~ IdRest(dollar) )

  def UppercaseId[_p: P](dollar: Boolean) = P( !Keywords ~ Upper ~ IdRest(dollar) )
  def PlainId[_p: P] = P( UppercaseId(true) | VarId | Operator ~ (!OpChar | &(StringIn("/*", "//"))) )
    .opaque("plain-id")

  def PlainIdNoDollar[_p: P] = P( UppercaseId(false) | VarId0(false) | Operator ).opaque("plain-id")

  def BacktickId[_p: P] = P( "`" ~ CharsWhile(NotBackTick) ~ "`" )
  def Id[_p: P]: P[Unit] = P( BacktickId | PlainId ).opaque("id")

  def IdRest[_p: P](allowDollar: Boolean) = {

    val IdCharacter =
      if(allowDollar) NamedFunction(c => c == '$' || isLetter(c) || isDigit(c))
      else NamedFunction(c => isLetter(c) || isDigit(c))

    def IdUnderscoreChunk = P( CharsWhileIn("_", 0) ~ CharsWhile(IdCharacter) )
    P( IdUnderscoreChunk.rep ~ (CharsWhileIn("_") ~ CharsWhile(isOpChar, 0)).? )
  }

  def AlphabetKeywords[_p: P] = P {
    StringIn("abstract", "case", "catch", "class", "def", "do", "else",
      "extends", "false", "finally", "final", "finally", "forSome",
      "for", "if", "implicit", "import", "lazy", "match", "new",
      "null", "object", "override", "package", "private", "protected",
      "return", "sealed", "super", "this", "throw", "trait", "try",
      "true", "type", "val", "var", "while", "with", "yield", "_", "macro") ~
      !CharPred(Basic.LetterDigitDollarUnderscore)
  }.opaque("AlphabetKeywords")

  def SymbolicKeywords[_p: P] = P{
    StringIn(":", ";", "=>", "=", "<-", "<:", "<%", ">:", "#", "@", "\u21d2", "\u2190") ~ !OpChar
  }.opaque("SymbolicKeywords")

  def Keywords[_p: P] = P( AlphabetKeywords | SymbolicKeywords )
}
