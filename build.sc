import mill._
import scalalib._
import scalajslib._
import publish._

val crossVersions = Seq("2.11.12", "2.12.7", "2.13.0-M5")
object fastparse extends Module{
  object jvm extends Cross[fastparseJvmModule](crossVersions:_*)
  class fastparseJvmModule(val crossScalaVersion: String) extends FastparseModule{
    def platformSegment = "jvm"
    object test extends Tests with CommonTestModule{
      def platformSegment = "jvm"
    }
    object bench extends ScalaModule{
      def scalaVersion = crossScalaVersion

      def moduleDeps = super.moduleDeps ++ Seq(fastparseJvmModule.this)
      def ivyDeps = Agg(
        ivy"com.lihaoyi::fastparse:1.0.0",
        ivy"com.lihaoyi::ammonite-ops:1.1.2",
        ivy"org.scala-lang:scala-reflect:${scalaVersion()}",
      )
    }

  }

  object js extends Cross[fastparseJsModule](crossVersions:_*)
  class fastparseJsModule(val crossScalaVersion: String) extends FastparseModule with ScalaJSModule {
    def platformSegment = "js"
    def scalaJSVersion = "0.6.25"
    object test extends Tests with CommonTestModule{
      def platformSegment = "js"
    }
  }
}
trait FastparseModule extends CommonCrossModule{
  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode::0.1.5",
  )
  def compileIvyDeps = Agg(
    ivy"org.scala-lang:scala-reflect:${scalaVersion()}"
  )
  def generatedSources = T{
    val dir = T.ctx().dest
    val file = dir/"fastparse"/"SequencerGen.scala"
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
      package fastparse
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

trait ExampleParseJsModule extends CommonCrossModule with ScalaJSModule{
  def moduleDeps = Seq(fastparse.js())
  def scalaJSVersion = "0.6.25"
  def platformSegment = "js"
  object test extends Tests with CommonTestModule{
    def platformSegment = "js"
  }
}


trait ExampleParseJvmModule extends CommonCrossModule{
  def moduleDeps = Seq(fastparse.jvm())
  def platformSegment = "jvm"
  object test extends Tests with CommonTestModule{
    def platformSegment = "jvm"
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"net.sourceforge.cssparser:cssparser:0.9.18",
      ivy"org.scala-lang:scala-compiler:${scalaVersion()}"
    )
  }
}

trait CommonCrossModule extends CrossScalaModule with PublishModule{

  def publishVersion = "2.0.5"
  def artifactName = millModuleSegments.parts.dropRight(2).mkString("-")
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.lihaoyi",
    url = "https://github.com/lihaoyi/fastparse",
    licenses = Seq(License.MIT),
    scm = SCM(
      "git://github.com/lihaoyi/fastparse.git",
      "scm:git://github.com/lihaoyi/fastparse.git"
    ),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
    )
  )

  def scalaDocPluginClasspath = T{ Agg[PathRef]() }
  def scalacOptions = T{ if (scalaVersion() == "2.12.7") Seq("-opt:l:method") else Nil }

  def platformSegment: String
  def millSourcePath = super.millSourcePath / ammonite.ops.up
  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )


}
trait CommonTestModule extends ScalaModule with TestModule{
  def platformSegment: String
  def ivyDeps = Agg(
    ivy"com.lihaoyi::utest::0.6.6",
  )

  def scalacOptions = T{ if (scalaVersion() == "2.12.7") Seq("-opt:l:method") else Nil }

  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )
  def testFrameworks = Seq("utest.runner.Framework")
}

object perftests extends Module{
  object bench1 extends PerfTestModule {
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.lihaoyi::scalaparse:1.0.0",
      ivy"com.lihaoyi::pythonparse:1.0.0",
      ivy"com.lihaoyi::cssparse:1.0.0",
    )
  }

  object bench2 extends PerfTestModule {
    def moduleDeps = Seq(
      scalaparse.jvm("2.12.7").test,
      pythonparse.jvm("2.12.7").test,
      cssparse.jvm("2.12.7").test,
      fastparse.jvm("2.12.7").test,
    )

  }


  object json extends PerfTestModule {
    def moduleDeps = Seq(
      fastparse.jvm("2.12.7").test,
      scalaparse.jvm("2.12.7").test,
      pythonparse.jvm("2.12.7").test
    )
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.json4s::json4s-ast:3.6.0",
      ivy"org.json4s::json4s-native:3.6.0",
      ivy"org.json4s::json4s-jackson:3.6.0",
      ivy"io.circe::circe-parser:0.9.1",
      ivy"io.argonaut::argonaut:6.2",
      ivy"com.typesafe.play::play-json:2.6.9",
      ivy"com.fasterxml.jackson.core:jackson-databind:2.9.4",
      ivy"com.lihaoyi::ujson:0.6.7",
      ivy"org.scala-lang.modules::scala-parser-combinators:1.1.1",
      ivy"org.python:jython:2.7.1b3"
    )
  }

  trait PerfTestModule extends ScalaModule with TestModule{
    def scalaVersion = "2.12.7"
    def scalacOptions = Seq("-opt:l:method")
    def resources = T.sources{
      Seq(PathRef(perftests.millSourcePath / "resources")) ++
        fastparse.jvm("2.12.7").test.resources()
    }
    def testFrameworks = Seq("utest.runner.Framework")
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.6.6",
      ivy"org.scala-lang:scala-compiler:${scalaVersion()}"
    )
  }
}

object demo extends ScalaJSModule{
  def scalaJSVersion = "0.6.25"
  def scalaVersion = "2.12.7"
  def moduleDeps = Seq(
    scalaparse.js("2.12.7"),
    cssparse.js("2.12.7"),
    pythonparse.js("2.12.7"),
    fastparse.js("2.12.7").test,
  )
  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::0.9.2",
    ivy"com.lihaoyi::scalatags::0.6.5"
  )
}
