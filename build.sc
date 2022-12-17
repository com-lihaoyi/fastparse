import mill._
import scalalib._
import scalajslib._
import scalanativelib._
import publish._
import mill.eval.Result
import mill.modules.Jvm.createJar
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.1.4`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import $ivy.`com.github.lolgab::mill-mima::0.0.13`
import com.github.lolgab.mill.mima._

val scala211 = "2.11.12"
val scala212 = "2.12.13"
val scala213 = "2.13.6"
val scalaJS1 = "1.7.1"
val scalaNative04 = "0.4.9"

val crossVersions = Seq(scala213, scala212, scala211)
val crossJsVersions = Seq(scala213 -> scalaJS1, scala212 -> scalaJS1, scala211 -> scalaJS1)
val crossNativeVersions = Seq(scala213 -> scalaNative04, scala212 -> scalaNative04, scala211 -> scalaNative04)

object fastparse extends Module{
  object jvm extends Cross[fastparseJvmModule](crossVersions:_*)
  class fastparseJvmModule(val crossScalaVersion: String) extends FastparseModule{
    def platformSegment = "jvm"
    object test extends Tests with CommonTestModule{
      def platformSegment = "jvm"
    }
  }

  object js extends Cross[fastparseJsModule](crossJsVersions:_*)
  class fastparseJsModule(val crossScalaVersion: String, crossScalaJsVersion: String) extends FastparseModule with ScalaJSModule {
    def platformSegment = "js"
    def millSourcePath = super.millSourcePath / os.up
    def scalaJSVersion = crossScalaJsVersion
    object test extends Tests with CommonTestModule{
      def platformSegment = "js"
    }
  }

  object native extends Cross[fastparseNativeModule](crossNativeVersions:_*)
  class fastparseNativeModule(val crossScalaVersion: String, crossScalaNativeVersion: String) extends FastparseModule with ScalaNativeModule {
    def platformSegment = "native"
    def millSourcePath = super.millSourcePath / os.up
    def scalaNativeVersion = crossScalaNativeVersion

    object test extends Tests with CommonTestModule{
      def platformSegment = "native"
    }
  }
}

trait FastparseModule extends CommonCrossModule{
  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode::0.2.3",
    ivy"com.lihaoyi::geny::0.6.10"
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
    os.write(file, output, createFolders = true)
    Seq(PathRef(file))
  }
}

object scalaparse extends Module{
  object js extends Cross[ScalaParseJsModule](crossJsVersions:_*)
  class ScalaParseJsModule(val crossScalaVersion: String, val crossScalaJsVersion: String) extends ExampleParseJsModule

  object jvm extends Cross[ScalaParseJvmModule](crossVersions:_*)
  class ScalaParseJvmModule(val crossScalaVersion: String) extends ExampleParseJvmModule

  object native extends Cross[ScalaParseNativeModule](crossNativeVersions:_*)
  class ScalaParseNativeModule(val crossScalaVersion: String, val crossScalaNativeVersion: String) extends ExampleParseNativeModule
}

object cssparse extends Module{
  object js extends Cross[CssParseJsModule](crossJsVersions:_*)
  class CssParseJsModule(val crossScalaVersion: String, val crossScalaJsVersion: String) extends ExampleParseJsModule

  object jvm extends Cross[CssParseJvmModule](crossVersions:_*)
  class CssParseJvmModule(val crossScalaVersion: String) extends ExampleParseJvmModule

  object native extends Cross[CssParseNativeModule](crossNativeVersions:_*)
  class CssParseNativeModule(val crossScalaVersion: String, val crossScalaNativeVersion: String) extends ExampleParseNativeModule
}

object pythonparse extends Module{
  object js extends Cross[PythonParseJsModule](crossJsVersions:_*)
  class PythonParseJsModule(val crossScalaVersion: String, val crossScalaJsVersion: String) extends ExampleParseJsModule

  object jvm extends Cross[PythonParseJvmModule](crossVersions:_*)
  class PythonParseJvmModule(val crossScalaVersion: String) extends ExampleParseJvmModule

  object native extends Cross[PythonParseNativeModule](crossNativeVersions:_*)
  class PythonParseNativeModule(val crossScalaVersion: String, val crossScalaNativeVersion: String) extends ExampleParseNativeModule
}

trait ExampleParseJsModule extends CommonCrossModule with ScalaJSModule{
  def moduleDeps = Seq(fastparse.js(crossScalaVersion, crossScalaJsVersion))
  def crossScalaJsVersion: String
  def scalaJSVersion = crossScalaJsVersion
  def platformSegment = "js"
  def millSourcePath = super.millSourcePath / os.up
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

trait ExampleParseNativeModule extends CommonCrossModule with ScalaNativeModule{
  def platformSegment = "native"
  def crossScalaNativeVersion: String
  def scalaNativeVersion = crossScalaNativeVersion
  def millSourcePath = super.millSourcePath / os.up
  def moduleDeps = Seq(fastparse.native(crossScalaVersion, crossScalaNativeVersion))
  object test extends Tests with CommonTestModule{
    def platformSegment = "native"
  }
}



trait CommonCrossModule extends CrossScalaModule with PublishModule with Mima{

  def publishVersion = VcsVersion.vcsState().format()
  def mimaPreviousVersions = Seq(
    VcsVersion
      .vcsState()
      .lastTag
      .getOrElse(throw new Exception("Missing last tag"))
  )
  def mimaBinaryIssueFilters = super.mimaBinaryIssueFilters() ++ Seq(
    ProblemFilter.exclude[IncompatibleResultTypeProblem]("fastparse.Parsed#Failure.unapply")
  )
  def artifactName = millModuleSegments.parts.dropRight(2).mkString("-").stripSuffix(s"-$platformSegment")
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.lihaoyi",
    url = "https://github.com/lihaoyi/fastparse",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github(
      "com-lihaoyi",
      "fastparse"
    ),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
    )
  )

  def scalaDocPluginClasspath = T{ Agg[PathRef]() }
//  def scalacOptions = T{ if (scalaVersion() == "2.12.10") Seq("-opt:l:method") else Nil }

  def platformSegment: String
  def millSourcePath = super.millSourcePath / os.up
  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )


}
trait CommonTestModule extends ScalaModule with TestModule.Utest{

  def platformSegment: String
  def ivyDeps = Agg(
    ivy"com.lihaoyi::utest::0.7.11",
  )

//  def scalacOptions = T{ if (scalaVersion() == "2.12.10") Seq("-opt:l:method") else Nil }

  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )
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
      scalaparse.jvm(scala212).test,
      pythonparse.jvm(scala212).test,
      cssparse.jvm(scala212).test,
      fastparse.jvm(scala212).test,
    )

  }


  object compare extends PerfTestModule {
    def moduleDeps = Seq(
      fastparse.jvm(scala212).test,
      scalaparse.jvm(scala212).test,
      pythonparse.jvm(scala212).test
    )
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.json4s::json4s-ast:3.6.0",
      ivy"org.json4s::json4s-native:3.6.0",
      ivy"org.json4s::json4s-jackson:3.6.0",
      ivy"io.circe::circe-parser:0.9.1",
      ivy"io.argonaut::argonaut:6.2",
      ivy"com.typesafe.play::play-json:2.6.9",
      ivy"com.fasterxml.jackson.core:jackson-databind:2.9.4",
      ivy"com.lihaoyi::ujson:1.1.0",
      ivy"org.scala-lang.modules::scala-parser-combinators:1.1.1",
      ivy"org.python:jython:2.7.1b3"
    )
  }

  trait PerfTestModule extends ScalaModule with TestModule.Utest{
    def scalaVersion = scala212
    def scalacOptions = Seq("-opt:l:method")
    def resources = T.sources{
      Seq(PathRef(perftests.millSourcePath / "resources")) ++
        fastparse.jvm(scala212).test.resources()
    }
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.11",
      ivy"org.scala-lang:scala-compiler:${scalaVersion()}"
    )
  }
}

object demo extends ScalaJSModule{
  def scalaJSVersion = scalaJS1
  def scalaVersion = scala213
  def moduleDeps = Seq(
    scalaparse.js(scala213, scalaJS1),
    cssparse.js(scala213, scalaJS1),
    pythonparse.js(scala213, scalaJS1),
    fastparse.js(scala213, scalaJS1).test,
  )
  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::0.9.8",
    ivy"com.lihaoyi::scalatags::0.9.3"
  )
}
