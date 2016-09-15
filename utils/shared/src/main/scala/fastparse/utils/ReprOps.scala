package fastparse.utils

/**
  * Encapsulates all the common operations on each [[Elem]] and [[Repr]] that
  * FastParse needs to perform it's core functionality. This is provided
  * separately, in order to avoid converting every possible input into a
  * lowest-common-denominator type (e.g. `IndexedSeq[Elem]`) to avoid
  * unnecessarily paying conversion-costs and copying the input.
  */
abstract class ReprOps[Elem, Repr] {
  def prettyPrint(input: Repr): String
  def literalize(input: Repr): String
  def errorMessage(input: ParserInput[Elem, Repr], expected: String, idx: Int): String
  def prettyIndex(input: ParserInput[Elem, Repr], index: Int): String

  def slice0(value: Repr, start: Int, end: Int): Repr
  def apply0(value: Repr, i: Int): Elem
  def length0(value: Repr): Int
  def fromArray(input: Array[Elem]): Repr
  def fromSeq(input: Seq[Elem]): Repr
  def fromSingle(input: Elem): Repr
  def toArray(input: Repr): Array[Elem]
  def flatten(input: Seq[Repr]): Repr
}


