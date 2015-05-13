package fastparse
package core
import acyclic.file
import fastparse.Utils._
/**
 * Result of a parse, whether successful or failed
 */
sealed trait Result[+T]

object Result{
  case class Frame(index: Int, parser: Parser[_])
  trait Success[T] extends Result[T]{
    /**
     * The result of this parse
     */
    def value: T
    /**
     * The index where the parse completed; may be less than
     * the length of input
     */
    def index: Int
    /**
     * Whether or not this parse encountered a Cut
     */
    def cut: Boolean
  }
  object Success{
    case class Mutable[T](var value: T, var index: Int, var cut: Boolean = false) extends Success[T]{
      override def toString = s"Success($value, $index)"
    }
    def unapply[T](x: Result[T]) = x match{
      case s: Success[T] => Some((s.value, s.index))
      case _ => None
    }
  }

  trait Failure extends Result[Nothing]{
    /**
     * The input string for the failed parse. Useful so the [[Failure]]
     * object can pretty-print snippet
     */
    def input: String

    /**
     * The entire stack trace where the parse failed, containing every
     * parser in the stack and the index where the parser was used, excluding
     * the final parser and index where the parse failed
     */
    def fullStack: List[Frame]
    /**
     * A slimmed down version of [[fullStack]], this only includes named
     * [[parsers.Combinators.Rule]] objects as well as the final Parser (whether named or not)
     * and index where the parse failed for easier reading.
     */
    def stack: List[Frame]

    /**
     * The index in the parse where this parse failed
     */
    def index: Int

    /**
     * The deepest parser in the parse which failed
     */
    def parser: Parser[_]

    /**
     * A one-line snippet that tells you what the state of the
     * parser was when it failed
     */
    def trace: String
    /**
     * A longer version of [[trace]], which shows more context
     * for every stack frame
     */
    def verboseTrace: String

    /**
     * Whether or not this parse encountered a Cut
     */
    def cut: Boolean
  }
  object Failure {
    def unapply[T](x: Result[T]) = x match{
      case s: Failure => Some((s.parser, s.index))
      case _ => None
    }
    case class Mutable(var input: String,
                       var fullStack: List[Frame],
                       var index: Int,
                       var parser: Parser[_],
                       var cut: Boolean) extends Failure {

      def stack = fullStack.collect {
        case f@Frame(i, p) if p.shortTraced => f
      } :+ Frame(index, parser)

      def verboseTrace = {
        val body =
          for (Frame(index, p) <- stack)
            yield s"$index\t...${literalize(input.slice(index, index + 5))}\t$p"
        body.mkString("\n")
      }

      def trace = {
        val body =
          for (Frame(index, p) <- stack)
            yield s"$p:$index"

        body.mkString(" / ") + " ..." + literalize(input.slice(index, index + 10))
      }

      override def toString = s"Failure($trace, $cut)"
    }

  }
}
import fastparse.core.Result._

/**
 * Things which get passed through the entire parse run, but almost never
 * get changed in the process.
 *
 * @param input The string that is currently being parsed
 * @param logDepth
 * @param trace
 */
case class ParseCtx(input: String, logDepth: Int, trace: Boolean){
  val failure = Failure.Mutable(input, Nil, 0, null, false)
  val success = Success.Mutable(null, 0, false)
}


/**
 * A single, self-contained, immutable parser. The primary method is
 * `parse`, which returns a [[T]] on success and a stack trace on failure.
 *
 * Some small optimizations are performed in-line: collapsing [[parsers.Combinators.Either]]
 * cells into large ones and collapsing [[parsers.Combinators.Sequence]] cells into
 * [[parsers.Combinators.Sequence.Flat]]s. These optimizations together appear to make
 * things faster but any 10%, whether or not you activate tracing
 *
 * Collapsed, Trace   Timings
 * all        true    87 /97 /94
 * all        false   112/111/104
 * either     true    89 /84 /81
 * either     false   97 /102/97
 * none       true    84 /79 /80
 * none       false   96 /99 /97
 */
trait Parser[+T] extends ParserApi[T]{
  /**
   * Parses the given `input` starting from the given `index`
   *
   * @param input The string we want to parse
   *
   * @param index The index in the string to start from. By default parsing
   *              starts from the beginning of a string, but you can start
   *              from halfway through the string if you want.
   *
   * @param trace Whether or not you want a full trace of any error messages
   *              that appear. Without it, you only get the single deepest
   *              parser in the call-stack when it failed, and its index. With
   *              `trace`, you get every parser all the way to the top, though
   *              this comes with a ~20-40% slowdown.
   */
  def parse(input: String, index: Int = 0, trace: Boolean = true): Result[T] = {
    parseRec(ParseCtx(input, 0, trace), index)
  }

  /**
   * Parses the given `input` starting from the given `index` and `logDepth`
   */
  def parseRec(cfg: ParseCtx, index: Int): Result[T]

  /**
   * Whether or not this parser should show up when [[Failure.trace]] is called
   */
  def shortTraced: Boolean = false
}

trait ParserApi[+T]{ this: Parser[T] =>
  def fail(f: Failure.Mutable, index: Int, cut: Boolean = false) = {
    f.index = index
    f.cut = cut
    f.fullStack = Nil
    f.parser = this
    f
  }

  def failMore(f: Failure.Mutable, index: Int, trace: Boolean, cut: Boolean = false) = {
    if (trace) f.fullStack = new ::(new Result.Frame(index, this), f.fullStack)
    f.cut = f.cut | cut
    f
  }
  def success[T](s: Success.Mutable[_], value: T, index: Int, cut: Boolean) = {
    val s1 = s.asInstanceOf[Success.Mutable[T]]
    s1.value = value
    s1.index = index
    s1.cut = cut
    s1
  }
}