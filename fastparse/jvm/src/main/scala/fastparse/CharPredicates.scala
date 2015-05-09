
package fastparse

object CharPredicates{
  lazy val isLetter = (c: Char) => c.isLetter
  lazy val isDigit = (c: Char) => c.isDigit
  lazy val isPrintableChar = (c: Char) => {
    val block = java.lang.Character.UnicodeBlock.of(c)
    !java.lang.Character.isISOControl(c) &&
    !java.lang.Character.isSurrogate(c) &&
    block != null && block !=
    java.lang.Character.UnicodeBlock.SPECIALS
  }
  lazy val isMathSymbol = (c: Char) => c.getType == Character.MATH_SYMBOL
  lazy val isOtherSymbol = (c: Char) => c.getType == Character.OTHER_SYMBOL
  lazy val isUpper = (c: Char) => c.isUpper
  lazy val isLower = (c: Char) => c.isLower
}