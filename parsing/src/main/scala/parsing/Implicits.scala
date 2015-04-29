package parsing

/**
 * Container for all the type-level logic around appending things
 * to tuples or flattening `Seq[Unit]`s into `Unit`s
 */
object Implicits {
  trait Sequencer[-T, V, R]{
    def apply(t: T, v: V): R
  }
  object Sequencer extends LowPriSequencer{
    def apply[T, V, R](f: (T, V) => R) = new Sequencer[T, V, R]{
      def apply(t: T, v: V): R = f(t, v)
    }
    implicit def SingleSequencer[T]: Sequencer[Unit, T, T] = Sequencer{case ((), t) => t }
  }
  trait LowPriSequencer extends LowerPriSequencer{
    implicit def UnitSequencer[T]: Sequencer[T, Unit, T] = Sequencer{case ((t), ()) => t }
  }
  trait LowerPriSequencer extends LowestPriSequencer{
    implicit def Sequencer2[T1, T2, D]: Sequencer[(T1, T2), D, (T1, T2, D)] = Sequencer{case ((t1, t2), d) => (t1, t2, d) }
    implicit def Sequencer3[T1, T2, T3, D]: Sequencer[(T1, T2, T3), D, (T1, T2, T3, D)] = Sequencer{case ((t1, t2, t3), d) => (t1, t2, t3, d) }
    implicit def Sequencer4[T1, T2, T3, T4, D]: Sequencer[(T1, T2, T3, T4), D, (T1, T2, T3, T4, D)] = Sequencer{case ((t1, t2, t3, t4), d) => (t1, t2, t3, t4, d) }
    implicit def Sequencer5[T1, T2, T3, T4, T5, D]: Sequencer[(T1, T2, T3, T4, T5), D, (T1, T2, T3, T4, T5, D)] = Sequencer{case ((t1, t2, t3, t4, t5), d) => (t1, t2, t3, t4, t5, d) }
    implicit def Sequencer6[T1, T2, T3, T4, T5, T6, D]: Sequencer[(T1, T2, T3, T4, T5, T6), D, (T1, T2, T3, T4, T5, T6, D)] = Sequencer{case ((t1, t2, t3, t4, t5, t6), d) => (t1, t2, t3, t4, t5, t6, d) }
    implicit def Sequencer7[T1, T2, T3, T4, T5, T6, T7, D]: Sequencer[(T1, T2, T3, T4, T5, T6, T7), D, (T1, T2, T3, T4, T5, T6, T7, D)] = Sequencer{case ((t1, t2, t3, t4, t5, t6, t7), d) => (t1, t2, t3, t4, t5, t6, t7, d) }
    implicit def Sequencer8[T1, T2, T3, T4, T5, T6, T7, T8, D]: Sequencer[(T1, T2, T3, T4, T5, T6, T7, T8), D, (T1, T2, T3, T4, T5, T6, T7, T8, D)] = Sequencer{case ((t1, t2, t3, t4, t5, t6, t7, t8), d) => (t1, t2, t3, t4, t5, t6, t7, t8, d) }
  }
  trait LowestPriSequencer{
    implicit def Sequencer1[T1, T2]: Sequencer[T1, T2, (T1, T2)] = Sequencer{case (t1, t2) => (t1, t2)}
  }
  trait Repeater[-T, R]{
    def apply(i: Iterator[T]): R
  }
  object Repeater extends LowPriRepeater{
    def apply[T, R](f: Iterator[T] => R) = new Repeater[T, R]{
      def apply(i: Iterator[T]): R = f(i)
    }
    implicit def UnitRepeater = Repeater[Unit, Unit](_ => ())
  }
  trait LowPriRepeater{
    implicit def GenericRepeater[T] = Repeater[T, Seq[T]](_.toVector)
  }

  trait Optioner[-T, R]{
    def apply(i: Option[T]): R
  }
  object Optioner extends LowPriOptioner{
    def apply[T, R](f: Option[T] => R) = new Optioner[T, R]{
      def apply(i: Option[T]): R = f(i)
    }
    implicit def UnitOptioner = Optioner[Unit, Unit](_ => ())
  }
  trait LowPriOptioner{
    implicit def GenericOptioner[T] = Optioner[T, Option[T]](x => x)
  }
}
