package fastparse.parsers
import acyclic.file
import fastparse.utils.Utils._
import fastparse.core.{ParseCtx, Parsed, Parser, Precedence}
import fastparse.utils.{ElemSetHelper, Generator, ReprOps, Utils}

/**
 * High-performance intrinsics for parsing common patterns. All
 * of these have equivalent to constructs that can be put together
 * using a combination of "string"s, p1 | p2, and p.rep, but much
 * faster or more convenient.
 */
object Intrinsics {

  abstract class ElemSet[Elem, Repr](generatorOrPred: GenOrPred[Elem])
                                    (implicit helper: ElemSetHelper[Elem],
                                     repr: ReprOps[Elem, Repr])
      extends PrecomputableParser[Elem, Repr](generatorOrPred){


    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      val input = cfg.input
      if (!input.isReachable(index)) fail(cfg.failure, index)
      else if (check(input(index))) success(cfg.success, (), index + 1, Set.empty, false)
      else fail(cfg.failure, index)
    }
  }




  /**
   * Parses a single character if it passes the predicate
   */
  case class ElemPred[Elem, Repr](name: String,
                                  predicate: Elem => Boolean,
                                  precompute: Boolean)
                                 (implicit helper: ElemSetHelper[Elem],
                                  repr: ReprOps[Elem, Repr])
    extends ElemSet[Elem, Repr](makeGenOrPred(predicate, precompute)){

    override def toString = s"$name($predicate)"
  }

  def flattenStringsGen[Elem](items: Seq[Seq[Elem]]) = {
    new Generator.Iter(items.iterator.map(_.iterator).flatten)
  }
  /**
   * Parses a single character if its contained in the lists of allowed characters
   */
  case class ElemIn[Elem, Repr](name: String,
                                strings: Seq[Seq[Elem]])
                               (implicit repr: ReprOps[Elem, Repr],
                                ehelper: ElemSetHelper[Elem])
      extends ElemSet[Elem, Repr](Left(flattenStringsGen(strings))){
    override def toString = prettyPrintStrings(name, strings)
  }

  /**
    * Keeps consuming characters as long as they are within
    * [[strings]]
    */
  case class ElemsWhileIn[Elem, Repr](name: String,
                                      strings: Seq[Seq[Elem]],
                                      min: Int = 1)
                                     (implicit helper: ElemSetHelper[Elem],
                                      repr: ReprOps[Elem, Repr])
    extends PrecomputableParser[Elem, Repr](Left(flattenStringsGen(strings)))
    with WhileParser[Elem, Repr]{

    override def toString = prettyPrintStrings(name, strings)
  }

  /**
   * Keeps consuming characters until the predicate [[predicate]] becomes false.
   * Functionally equivalent to using `.rep` and [[ElemPred]], but much faster.
   */
  case class ElemsWhile[Elem, Repr](name: String,
                                    predicate: Elem => Boolean,
                                    min: Int = 1,
                                    precompute: Boolean)
                                   (implicit helper: ElemSetHelper[Elem],
                                    repr: ReprOps[Elem, Repr])
      extends PrecomputableParser[Elem, Repr](makeGenOrPred(predicate, precompute))
      with WhileParser[Elem, Repr]{

    override def toString = s"$name($predicate)"
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
      extends Parser[Elem, Repr, Unit] {

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




  // ------------------------------ Helpers ------------------------------


  def prettyPrintStrings[Elem, Repr](name: String,
                                     strings: Seq[Seq[Elem]])
                                    (implicit repr: ReprOps[Elem, Repr]) = {

    s"$name(${repr.literalize(repr.flatten(strings.map(repr.fromSeq)))})"
  }
  trait WhileParser[Elem, Repr] extends Parser[Elem, Repr, Unit]{
    def check(e: Elem): Boolean
    def min: Int
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      var curr = index
      val input = cfg.input
      while(input.isReachable(curr) && check(input(curr))) curr += 1
      if (curr - index < min) fail(cfg.failure, curr)
      else success(cfg.success, (), curr, Set.empty, false)
    }
  }

  type GenOrPred[Elem] = Either[Generator[Elem], Elem => Boolean]
  abstract class PrecomputableParser[Elem, Repr](generatorOrPred: GenOrPred[Elem])
                                                (implicit val helper: ElemSetHelper[Elem],
                                                 val repr: ReprOps[Elem, Repr])
    extends Parser[Elem, Repr, Unit]{

    private[this] val uberSet: Utils.BitSet[Elem] = generatorOrPred match{
      case Left(generator) => BitSet(generator)
      case Right(pred) => null
    }

    private[this] val precompute0 = generatorOrPred.isLeft
    private[this] val predicate0: Elem => Boolean = generatorOrPred match{
      case Left(generator) => null
      case Right(pred) => pred
    }

    def check(e: Elem) = if (precompute0) uberSet(e) else predicate0(e)
  }


  def makeGenOrPred[Elem](predicate: Elem => Boolean, precompute: Boolean)
                         (implicit helper: ElemSetHelper[Elem]): GenOrPred[Elem] = {
    if (precompute) Left(new Generator.Pred(predicate))
    else Right(predicate)
  }
}
