package fastparse
package preds
object CharPredicates{
lazy val isLetter = (c: Char) => c.isLetter
  lazy val isDigit = (c: Char) => c.isDigit
  lazy val isPrintableChar = (c: Char) => Utils.isPrintableChar(c)
  lazy val isMathSymbol = (c: Char) => c.getType == Character.MATH_SYMBOL
  lazy val isOtherSymbol = (c: Char) => c.getType == Character.OTHER_SYMBOL
  lazy val isUpper = (c: Char) => c.isUpper
  lazy val isLower = (c: Char) => c.isLower
}