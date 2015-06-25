package fastparse.parsers
import fastparse.core.Mutable
import fastparse.core.Parser
import fastparse.core.Result.{Failure, Success}
import fastparse.core.{ParseCtx, Result}

/**
 * Parsers that work with the output of a successful parse
 */
object Transformers {
  /**
   * Applies a transformation [[f]] to the result of [[p]]
   */
  case class Mapper[T, V](p: Parser[T], f: T => V) extends Parser[V]{
    def parseRec(cfg: ParseCtx, index: Int) = {
      p.parseRec(cfg, index) match{
        case s: Mutable.Success[T] => success(s, f(s.value), s.index, s.traceParsers, s.cut)
        case f: Mutable.Failure => failMore(f, index, cfg.trace, f.traceParsers)
      }
    }
    override def toString = p.toString
  }

  case class FlatMapped[T, V](p1: Parser[T], func: T => Parser[V])
    extends Parser[V] {
    def parseRec(cfg: ParseCtx, index: Int) = {
      p1.parseRec(cfg, index) match{
        case f: Mutable.Failure => failMore(f, index, cfg.trace, f.traceParsers, cut = false)
        case s: Mutable.Success[T] => func(s.value).parseRec(cfg, s.index)
      }
    }
  }

  case class Filtered[T](p: Parser[T], predicate: T => Boolean)
    extends Parser[T] {
    override def parseRec(cfg: ParseCtx, index: Int) = {
      p.parseRec(cfg, index) match{
        case f: Mutable.Failure => failMore(f, index, cfg.trace, f.traceParsers, cut = false)
        case s: Mutable.Success[T] =>
          if (predicate(s.value)) s else fail(cfg.failure,index, cfg.trace, s.traceParsers, cut = false)
      }
    }

    override def toString: String = s"$p.filter($predicate)"
  }
}
