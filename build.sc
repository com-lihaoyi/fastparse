import mill._, scalalib._
object fasterparser extends ScalaModule{
  def scalaVersion = "2.12.7"
  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode:0.1.4",
    ivy"org.scala-lang:scala-reflect:${scalaVersion()}"
  )

  def generatedSources = T{
    val dir = T.ctx().dest
    val file = dir/"fasterparser"/"SequencerGen.scala"
    // Only go up to 21, because adding the last element makes it 22
    val tuples = (2 to 21).map{ i =>
      val ts = (1 to i) map ("T" + _)
      val chunks = (1 to i) map { n =>
        s"t._$n"
      }
      val tsD = (ts :+ "D").mkString(",")
      s"""
          implicit def Sequencer$i[$tsD]: Sequencer[(${ts.mkString(", ")}), D, ($tsD)] =
            Sequencer0((t, d) => (${chunks.mkString(", ")}, d))
          """
    }
    val output = s"""
      package fasterparser
      trait SequencerGen[Sequencer[_, _, _]] extends LowestPriSequencer[Sequencer]{
        protected[this] def Sequencer0[A, B, C](f: (A, B) => C): Sequencer[A, B, C]
        ${tuples.mkString("\n")}
      }
      trait LowestPriSequencer[Sequencer[_, _, _]]{
        protected[this] def Sequencer0[A, B, C](f: (A, B) => C): Sequencer[A, B, C]
        implicit def Sequencer1[T1, T2]: Sequencer[T1, T2, (T1, T2)] = Sequencer0{case (t1, t2) => (t1, t2)}
      }
    """.stripMargin
    ammonite.ops.write(file, output)
    Seq(PathRef(file))
  }
  object test extends Tests{
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.6.5",
    )
    def testFrameworks = Seq("utest.runner.Framework")
  }
  object bench extends ScalaModule{
    def scalaVersion = "2.12.7"

    override def scalacOptions = Seq("-Ydebug")
    def moduleDeps = super.moduleDeps ++ Seq(fasterparser)
    def ivyDeps = Agg(
      ivy"com.lihaoyi::fastparse:1.0.0",
      ivy"com.lihaoyi::ammonite-ops:1.1.2",
      ivy"org.scala-lang:scala-reflect:${scalaVersion()}",
    )
  }
}