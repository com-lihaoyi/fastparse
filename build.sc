import mill._, scalalib._, scalajslib._

val crossVersions = Seq("2.11.12", "2.12.7")
object fasterparser extends Module{
  object jvm extends Cross[FasterParserJvmModule](crossVersions:_*)
  class FasterParserJvmModule(val crossScalaVersion: String) extends FasterParserModule

  object js extends Cross[FasterParserJsModule](crossVersions:_*)
  class FasterParserJsModule(val crossScalaVersion: String) extends FasterParserModule with ScalaJSModule {
    def scalaJSVersion = "0.6.25"
  }
}
trait FasterParserModule extends CrossScalaModule{
  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode:0.1.4",
  )
  def compileIvyDeps = Agg(
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
      val anys = ts.map(_ => "Any").mkString(", ")
      s"""
          val BaseSequencer$i: Sequencer[($anys), Any, ($anys, Any)] =
            Sequencer0((t, d) => (${chunks.mkString(", ")}, d))
          implicit def Sequencer$i[$tsD]: Sequencer[(${ts.mkString(", ")}), D, ($tsD)] =
            BaseSequencer$i.asInstanceOf[Sequencer[(${ts.mkString(", ")}), D, ($tsD)]]
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
    def scalaVersion = crossScalaVersion

    override def scalacOptions = Seq("-opt:l:method")

    def moduleDeps = super.moduleDeps ++ Seq(FasterParserModule.this)
    def ivyDeps = Agg(
      ivy"com.lihaoyi::fastparse:1.0.0",
      ivy"com.lihaoyi::ammonite-ops:1.1.2",
      ivy"org.scala-lang:scala-reflect:${scalaVersion()}",
    )
  }
}

object scalaparse extends Module{
  object js extends Cross[ScalaParseJsModule](crossVersions:_*)
  class ScalaParseJsModule(val crossScalaVersion: String) extends ExampleParseJsModule

  object jvm extends Cross[ScalaParseJvmModule](crossVersions:_*)
  class ScalaParseJvmModule(val crossScalaVersion: String) extends ExampleParseJvmModule
}


object cssparse extends Module{
  object js extends Cross[CssParseJsModule](crossVersions:_*)
  class CssParseJsModule(val crossScalaVersion: String) extends ExampleParseJsModule

  object jvm extends Cross[CssParseJvmModule](crossVersions:_*)
  class CssParseJvmModule(val crossScalaVersion: String) extends ExampleParseJvmModule
}
object pythonparse extends Module{
  object js extends Cross[PythonParseJsModule](crossVersions:_*)
  class PythonParseJsModule(val crossScalaVersion: String) extends ExampleParseJsModule

  object jvm extends Cross[PythonParseJvmModule](crossVersions:_*)
  class PythonParseJvmModule(val crossScalaVersion: String) extends ExampleParseJvmModule
}

trait ExampleParseModule extends CrossScalaModule{
  object test extends Tests{
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:0.6.5",
    )

    def testFrameworks = Seq("utest.runner.Framework")
  }
}

trait ExampleParseJsModule extends ExampleParseModule{
  def moduleDeps = Seq(fasterparser.js())
  def scalaJSVersion = "0.6.25"

}


trait ExampleParseJvmModule extends ExampleParseModule{
  def moduleDeps = Seq(fasterparser.jvm())
}
