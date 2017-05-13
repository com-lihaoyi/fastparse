
package fastparse

object CharPredicates{
  lazy val isPrintableChar = (c: Char) => {
    val block = java.lang.Character.UnicodeBlock.of(c)
    !java.lang.Character.isISOControl(c) &&
    !java.lang.Character.isSurrogate(c) &&
    block != null && block !=
    java.lang.Character.UnicodeBlock.SPECIALS
  }
  def isMathSymbol(c: Char) = c.getType == Character.MATH_SYMBOL
  def isOtherSymbol(c: Char) = c.getType == Character.OTHER_SYMBOL
  def isLetter(c: Char) = c.isLetter
  def isDigit(c: Char) = c.isDigit
  def isLower(c: Char) = c.isLower
  def isUpper(c: Char) = c.isUpper
}