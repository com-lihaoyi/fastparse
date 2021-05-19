package fastparse

import scala.collection.mutable

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
    class NarySequencer[T, V, R](f: (T, V) => R) extends Sequencer[T, V, R]{
      def apply(t: T, v: V): R = f(t, v)
    }
    implicit def SingleSequencer[T]: Sequencer[Unit, T, T] = SingleSequencer0.asInstanceOf[Sequencer[Unit, T, T]]
    object SingleSequencer0 extends Sequencer[Unit, Any, Any]{
      def apply(t: Unit, v: Any): Any = v
    }
  }
  trait LowPriSequencer extends LowerPriSequencer{
    object UnitSequencer0 extends Sequencer[Any, Unit, Any]{
      def apply(t: Any, v: Unit): Any = t
    }
    implicit def UnitSequencer[T]: Sequencer[T, Unit, T] = UnitSequencer0.asInstanceOf[Sequencer[T, Unit, T]]
  }
  trait LowerPriSequencer extends SequencerGen[Sequencer]{
    protected[this] def Sequencer0[A, B, C](f: (A, B) => C) = new Sequencer.NarySequencer(f)
  }
  trait Repeater[-T, R]{
    type Acc
    def initial: Acc
    def accumulate(t: T, acc: Acc): Unit
    def result(acc: Acc): R
  }
  object Repeater extends LowPriRepeater{
    implicit object UnitRepeater extends Repeater[Unit, Unit]{
      type Acc = Unit
      def initial = ()
      def accumulate(t: Unit, acc: Unit) = acc
      def result(acc: Unit) = ()
    }
  }
  trait LowPriRepeater{
    implicit def GenericRepeaterImplicit[T]: Repeater[T, Seq[T]] = GenericRepeatedImplicit0.asInstanceOf[Repeater[T, Seq[T]]]
    object GenericRepeatedImplicit0 extends Repeater[Any, Seq[Any]]{
      type Acc = mutable.Buffer[Any]
      def initial = mutable.Buffer.empty[Any]
      def accumulate(t: Any, acc: mutable.Buffer[Any]) = acc += t
      def result(acc: mutable.Buffer[Any]) = acc.toSeq
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
    implicit def GenericOptionerImplicit[T]: Optioner[T, Option[T]] = GenericOptionerImplicit0.asInstanceOf[Optioner[T, Option[T]]]
    object GenericOptionerImplicit0 extends Optioner[Any, Option[Any]]{
      def none = None
      def some(value: Any) = Some(value)
    }
  }
}