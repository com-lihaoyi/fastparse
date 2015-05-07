package fastparse
package preds
object CharPredicates{
  lazy val isLetter = Utils.preCompute(_.isLetter)
  lazy val isDigit = Utils.preCompute(_.isDigit)
  lazy val isPrintableChar = Utils.preCompute(Utils.isPrintableChar)
  lazy val isMathSymbol = Utils.preCompute(_.getType == Character.MATH_SYMBOL)
  lazy val isOtherSymbol = Utils.preCompute(_.getType == Character.OTHER_SYMBOL)
  // Diverges between JS and JVM
  lazy val isUpper = Utils.preCompute(_.isUpper)
  lazy val isLower = Utils.preCompute(_.isLower)
}