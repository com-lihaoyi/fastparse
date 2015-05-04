package parsing

import scala.collection.mutable
import scala.runtime.IntRef

/**
 * Container for all the type-level logic around appending things
 * to tuples or flattening `Seq[Unit]`s into `Unit`s.
 *
 * Some of these implicits make liberal use of mutable state, so as
 * to minimize allocations while parsing.
 */
object Implicits {
  trait Sequencer[-T, V, R]{
    def apply(t: T, v: V): R
  }
  object Sequencer extends LowPriSequencer{
    def apply[T, V, R](f: (T, V) => R) = new Sequencer[T, V, R]{
      def apply(t: T, v: V): R = f(t, v)
    }
    implicit def SingleSequencer[T]: Sequencer[Unit, T, T] = Sequencer( (_, t) => t )
  }
  trait LowPriSequencer extends LowerPriSequencer{
    implicit def UnitSequencer[T]: Sequencer[T, Unit, T] = Sequencer( (t, _) => t )
  }
  trait LowerPriSequencer extends LowestPriSequencer{
    implicit def Sequencer2[T1, T2, D]: Sequencer[(T1, T2), D, (T1, T2, D)] = Sequencer( (t, d) => (t._1, t._2, d) )
    implicit def Sequencer3[T1, T2, T3, D]: Sequencer[(T1, T2, T3), D, (T1, T2, T3, D)] = Sequencer( (t, d) => (t._1, t._2, t._3, d) )
    implicit def Sequencer4[T1, T2, T3, T4, D]: Sequencer[(T1, T2, T3, T4), D, (T1, T2, T3, T4, D)] = Sequencer( (t, d) => (t._1, t._2, t._3, t._4, d) )
    implicit def Sequencer5[T1, T2, T3, T4, T5, D]: Sequencer[(T1, T2, T3, T4, T5), D, (T1, T2, T3, T4, T5, D)] = Sequencer( (t, d) => (t._1, t._2, t._3, t._4, t._5, d) )
    implicit def Sequencer6[T1, T2, T3, T4, T5, T6, D]: Sequencer[(T1, T2, T3, T4, T5, T6), D, (T1, T2, T3, T4, T5, T6, D)] = Sequencer( (t, d) => (t._1, t._2, t._3, t._4, t._5, t._6, d) )
    implicit def Sequencer7[T1, T2, T3, T4, T5, T6, T7, D]: Sequencer[(T1, T2, T3, T4, T5, T6, T7), D, (T1, T2, T3, T4, T5, T6, T7, D)] = Sequencer( (t, d) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, d) )
    implicit def Sequencer8[T1, T2, T3, T4, T5, T6, T7, T8, D]: Sequencer[(T1, T2, T3, T4, T5, T6, T7, T8), D, (T1, T2, T3, T4, T5, T6, T7, T8, D)] = Sequencer( (t, d) => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, d) )
  }
  trait LowestPriSequencer{
    implicit def Sequencer1[T1, T2]: Sequencer[T1, T2, (T1, T2)] = Sequencer{case (t1, t2) => (t1, t2)}
  }
  trait Repeater[-T, R]{
    def initial: R
    def accumulate(t: T, acc: R): Unit
  }
  object Repeater extends LowPriRepeater{
    implicit object UnitRepeater extends Repeater[Unit, Unit]{

      def initial = ()
      def accumulate(t: Unit, acc: Unit) = acc
    }
  }
  trait LowPriRepeater{
    implicit def GenericRepeaterImplicit[T] = GenericRepeater[T]()
    case class GenericRepeater[T]() extends Repeater[T, mutable.Buffer[T]]{
      def initial = mutable.Buffer.empty[T]
      def accumulate(t: T, acc: mutable.Buffer[T]) = acc += t
    }
  }

  trait Optioner[-T, R]{
    def none: R
    def some(value: T): R
  }
  
  object Optioner extends LowPriOptioner{
    implicit object UnitOptioner extends Optioner[Unit, Unit]{
      def none = ()
      def some(value: Unit) = ()
    }
  }
  trait LowPriOptioner{
    implicit def GenericOptionerImplicit[T] = GenericOptioner[T]()
    case class GenericOptioner[T]() extends Optioner[T, Option[T]]{
      def none = None
      def some(value: T) = Some(value)
    }
  }
}
