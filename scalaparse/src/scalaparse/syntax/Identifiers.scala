package scalaparse.syntax
import fasterparser.Parsing._
import fasterparser._
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

  def Operator[_: P] = P(
    !Keywords ~ (!("/*" | "//") ~ (CharsWhile(OpCharNotSlash) | "/")).rep(1)
  )

  def VarId[_: P] = VarId0(true)

  def VarId0[_: P](dollar: Boolean) = P( !Keywords ~ Lower ~ IdRest(dollar) )
  def PlainId[_: P] = P( !Keywords ~ Upper ~ IdRest(true) | VarId | Operator ~ (!OpChar | &("/*" | "//")) )
  def PlainIdNoDollar[_: P] = P( !Keywords ~ Upper ~ IdRest(false) | VarId0(false) | Operator )
  def BacktickId[_: P] = P( "`" ~ CharsWhile(NotBackTick) ~ "`" )
  def Id[_: P]: P[Unit] = P( BacktickId | PlainId )

  def IdRest[_: P](allowDollar: Boolean) = {

    val IdCharacter =
      if(allowDollar) NamedFunction(c => c == '$' || isLetter(c) || isDigit(c))
      else NamedFunction(c => isLetter(c) || isDigit(c))

    def IdUnderscoreChunk = P( CharsWhileIn("_", 0) ~ CharsWhile(IdCharacter) )
    P( IdUnderscoreChunk.rep ~ (CharsWhileIn("_") ~ CharsWhile(isOpChar, 0)).? )
  }

  def AlphabetKeywords[_: P] = P {
    StringIn("abstract", "case", "catch", "class", "def", "do", "else",
      "extends", "false", "finally", "final", "finally", "forSome",
      "for", "if", "implicit", "import", "lazy", "match", "new",
      "null", "object", "override", "package", "private", "protected",
      "return", "sealed", "super", "this", "throw", "trait", "try",
      "true", "type", "val", "var", "while", "with", "yield", "_", "macro") ~ !Letter
  }

  def SymbolicKeywords[_: P] = P{
    StringIn(":", ";", "=>", "=", "<-", "<:", "<%", ">:", "#", "@", "\u21d2", "\u2190") ~ !OpChar
  }

  def Keywords[_: P] = P( AlphabetKeywords | SymbolicKeywords )
}