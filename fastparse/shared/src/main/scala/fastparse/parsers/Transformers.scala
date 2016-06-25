package fastparse.parsers
import acyclic.file
import fastparse.core.Mutable
import fastparse.core.Parser
import fastparse.core.Parsed.{Failure, Success}
import fastparse.core.{ParseCtx, Parsed}

/**
 * Parsers that work with the output of a successful parse
 */
object Transformers {
  /**
   * Applies a transformation [[f]] to the result of [[p]]
   */
  case class Mapper[T, V, ElemType, R](p: Parser[T, ElemType, R], f: T => V) extends Parser[V, ElemType, R]{
    def parseRec(cfg: ParseCtx[ElemType], index: Int) = {
      p.parseRec(cfg, index) match{
        case s: Mutable.Success[T, ElemType] => success(s, f(s.value), s.index, s.traceParsers, s.cut)
        case f: Mutable.Failure[ElemType] => failMore(f, index, cfg.logDepth)
      }
    }
    override def toString = p.toString
  }

  case class FlatMapped[T, V, ElemType, R](p1: Parser[T, ElemType, R], func: T => Parser[V, ElemType, R])
    extends Parser[V, ElemType, R] {
    def parseRec(cfg: ParseCtx[ElemType], index: Int) = {
      p1.parseRec(cfg, index) match{
        case f: Mutable.Failure[ElemType] => failMore(f, index, cfg.logDepth, cut = false)
        case s: Mutable.Success[T, ElemType] => func(s.value).parseRec(cfg, s.index)
      }
    }
  }

  case class Filtered[T, ElemType, R](p: Parser[T, ElemType, R], predicate: T => Boolean)
    extends Parser[T, ElemType, R] {
    override def parseRec(cfg: ParseCtx[ElemType], index: Int) = {
      p.parseRec(cfg, index) match{
        case f: Mutable.Failure[ElemType] => failMore(f, index, cfg.logDepth, cut = false)
        case s: Mutable.Success[T, ElemType] =>
          if (predicate(s.value)) s
          else fail(cfg.failure,index, s.traceParsers, cut = false)
      }
    }

    override def toString: String = s"$p.filter($predicate)"
  }
}
