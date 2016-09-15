package fastparse.parsers
import acyclic.file
import fastparse.utils.Utils._
import fastparse.core.{ParseCtx, Parsed, Parser, Precedence}
import fastparse.utils.{ElemSetHelper, ReprOps, Utils}

/**
 * High-performance intrinsics for parsing common patterns. All
 * of these have equivalent to constructs that can be put together
 * using a combination of "string"s, p1 | p2, and p.rep, but much
 * faster or more convenient.
 */
object Intrinsics {

  abstract class ElemSet[Elem, Repr](elems: Seq[Elem])
                                    (implicit helper: ElemSetHelper[Elem],
                                     repr: ReprOps[Elem, Repr])
      extends Parser[Unit, Elem, Repr]{
    private[this] val uberSet = BitSet(elems)
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      val input = cfg.input
      if (!input.isReachable(index)) fail(cfg.failure, index)
      else if (uberSet(input(index))) success(cfg.success, (), index + 1, Set.empty, false)
      else fail(cfg.failure, index)
    }
  }
  /**
   * Parses a single character if it passes the predicate
   */
  case class ElemPred[Elem, Repr](name: String,
                                  predicate: Elem => Boolean)
                                 (implicit helper: ElemSetHelper[Elem],
                                  repr: ReprOps[Elem, Repr])
    extends ElemSet[Elem, Repr](helper.allValues.filter(predicate)){

    override def toString = s"$name($predicate)"
  }

  /**
   * Parses a single character if its contained in the lists of allowed characters
   */
  case class ElemIn[Elem, Repr](name: String,
                                strings: Seq[Elem]*)
                               (implicit repr: ReprOps[Elem, Repr],
                                ehelper: ElemSetHelper[Elem])
      extends ElemSet[Elem, Repr](repr.toArray(repr.flatten(strings.map(repr.fromSeq)))){
    override def toString = s"$name(${repr.literalize(repr.flatten(strings.map(repr.fromSeq)))})"
  }

  /**
   * Keeps consuming characters until the predicate [[pred]] becomes false.
   * Functionally equivalent to using `.rep` and [[ElemPred]], but much faster.
   */
  case class ElemsWhile[Elem, Repr](name: String,
                                        pred: Elem => Boolean, min: Int = 1)
                                       (implicit helper: ElemSetHelper[Elem],
                                        repr: ReprOps[Elem, Repr])
      extends Parser[Unit, Elem, Repr]{
    private[this] val uberSet = BitSet(helper.allValues.filter(pred))

    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      var curr = index
      val input = cfg.input
      while(input.isReachable(curr) && uberSet(input(curr))) curr += 1
      if (curr - index < min) fail(cfg.failure, curr)
      else success(cfg.success, (), curr, Set.empty, false)
    }
    override def toString = s"$name($pred)"
  }
  /**
   * Very efficiently attempts to parse a set of strings, by
   * first converting it into an array-backed Trie and then walking it once.
   * If multiple strings match the input, longest match wins.
   */
  class StringInBase[Elem, Repr](ignoreCase: Boolean, strings: Repr*)
                                (implicit repr: ReprOps[Elem, Repr],
                                 helper: ElemSetHelper[Elem],
                                 ordering: Ordering[Elem])
      extends Parser[Unit, Elem, Repr] {

    private[this] val trie = new TrieNode[Elem](strings.map(repr.toArray(_): IndexedSeq[Elem]), ignoreCase)

    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      val length = trie.query(cfg.input, index)
      if (length != -1) success(cfg.success, (), index + length + 1, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override def toString = {
      s"StringIn(${strings.map(repr.literalize).mkString(", ")})"
    }
  }

  case class StringIn[Elem, Repr](strings: Repr*)
                                 (implicit repr: ReprOps[Elem, Repr],
                                  helper: ElemSetHelper[Elem],
                                  ordering: Ordering[Elem]) extends StringInBase(false, strings:_*)

  case class StringInIgnoreCase[Elem, Repr](strings: Repr*)
                                           (implicit repr: ReprOps[Elem, Repr],
                                            helper: ElemSetHelper[Elem],
                                            ordering: Ordering[Elem]) extends StringInBase(true, strings:_*)
}
