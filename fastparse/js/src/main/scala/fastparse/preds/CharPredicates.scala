package fastparse
package preds
object CharPredicates{
  // Not available in Scala.js
  lazy val isLetter = Utils.preCompute(_.isLetter)
  lazy val isDigit = Utils.preCompute(_.isDigit)
  lazy val isPrintableChar = Utils.preCompute{c =>
    val block = java.lang.Character.UnicodeBlock.of(c)
    !java.lang.Character.isISOControl(c) &&
    !java.lang.Character.isSurrogate(c) &&
    block != null &&
    block != java.lang.Character.UnicodeBlock.SPECIALS
  }
  lazy val isMathSymbol = Utils.preCompute(_.getType == Character.MATH_SYMBOL)
  lazy val isOtherSymbol = Utils.preCompute(_.getType == Character.OTHER_SYMBOL)
  // Diverges between JS and JVM
  lazy val isUpper = Utils.preCompute(_.isUpper)
  lazy val isLower = Utils.preCompute(_.isLower)
}