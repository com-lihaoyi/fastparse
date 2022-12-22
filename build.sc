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
import $ivy.`com.github.lolgab::mill-crossplatform::0.1.1`
import com.github.lolgab.mill.crossplatform._

val scala211 = "2.11.12"
val scala212 = "2.12.13"
val scala213 = "2.13.6"
val scalaJS1 = "1.7.1"
val scalaNative04 = "0.4.9"

val crossScalaVersions = Seq(scala213, scala212, scala211)
val crossScalaJsVersions = Seq(scalaJS1)
val crossNativeVersions = Seq(scalaNative04)

object fastparse extends Cross[fastparseModule](crossScalaVersions: _*)
class fastparseModule(val crossScalaVersion: String) extends CrossPlatform {
  trait Shared extends CrossPlatformCrossScalaModule with CommonCrossModule {
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.lihaoyi::sourcecode::0.2.3",
      ivy"com.lihaoyi::geny::0.6.10"
    )
    def compileIvyDeps = super.ivyDeps() ++ Agg(
      ivy"org.scala-lang:scala-reflect:${scalaVersion()}"
    )
    def generatedSources = T {
      val dir = T.ctx().dest
      val file = dir / "fastparse" / "SequencerGen.scala"
      // Only go up to 21, because adding the last element makes it 22
      val tuples = (2 to 21).map { i =>
        val ts = (1 to i) map ("T" + _)
        val chunks = (1 to i) map { n =>
          s"t._$n"
        }
        val tsD = (ts :+ "D").mkString(",")
        val anys = ts.map(_ => "Any").mkString(", ")
        s"""
          val BaseSequencer$i: Sequencer[($anys), Any, ($anys, Any)] =
            Sequencer0((t, d) => (${chunks.mkString(", ")}, d))
          implicit def Sequencer$i[$tsD]: Sequencer[(${ts.mkString(
            ", "
          )}), D, ($tsD)] =
            BaseSequencer$i.asInstanceOf[Sequencer[(${ts.mkString(
            ", "
          )}), D, ($tsD)]]
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
  object jvm extends Shared {
    object test extends CommonTestModule with Tests
  }

  object js extends Cross[jsModule](crossScalaJsVersions: _*)
  class jsModule(val crossScalaJSVersion: String)
      extends Shared
      with CrossScalaJSModule {
    object test extends CommonTestModule with Tests
  }

  object native extends Cross[nativeModule](crossNativeVersions: _*)
  class nativeModule(val crossScalaNativeVersion: String)
      extends Shared
      with CrossScalaNativeModule {
    object test extends CommonTestModule with Tests
  }
}

object scalaparse extends Cross[ExampleParseModule](crossScalaVersions: _*)
object cssparse extends Cross[ExampleParseModule](crossScalaVersions: _*)
object pythonparse extends Cross[ExampleParseModule](crossScalaVersions: _*)

class ExampleParseModule(val crossScalaVersion: String) extends CrossPlatform {
  override def moduleDeps =
    super.moduleDeps ++ Seq(fastparse(crossScalaVersion))
  trait Shared extends CrossPlatformCrossScalaModule with CommonCrossModule

  object js extends Cross[jsModule](crossScalaJsVersions: _*)
  class jsModule(val crossScalaJSVersion: String)
      extends Shared
      with CrossScalaJSModule {
    object test extends Tests with CommonTestModule
  }

  object jvm extends Shared {
    object test extends Tests with CommonTestModule with CrossPlatformSources {
      def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"net.sourceforge.cssparser:cssparser:0.9.18",
        ivy"org.scala-lang:scala-compiler:${scalaVersion()}"
      )
    }
  }

  object native extends Cross[nativeModule](crossNativeVersions: _*)
  class nativeModule(val crossScalaNativeVersion: String)
      extends Shared
      with CrossScalaNativeModule {
    object test extends Tests with CommonTestModule
  }
}

trait CommonCrossModule extends CrossScalaModule with PublishModule with Mima {
  def publishVersion = VcsVersion.vcsState().format()
  def mimaPreviousVersions = Seq(
    VcsVersion
      .vcsState()
      .lastTag
      .getOrElse(throw new Exception("Missing last tag"))
  )
  def mimaBinaryIssueFilters = super.mimaBinaryIssueFilters() ++ Seq(
    ProblemFilter.exclude[IncompatibleResultTypeProblem](
      "fastparse.Parsed#Failure.unapply"
    )
  )
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
      Developer("lihaoyi", "Li Haoyi", "https://github.com/lihaoyi")
    )
  )

  def scalaDocPluginClasspath = T { Agg[PathRef]() }
//  def scalacOptions = T{ if (scalaVersion() == "2.12.10") Seq("-opt:l:method") else Nil }

}
trait CommonTestModule extends ScalaModule with TestModule.Utest {
  def ivyDeps = Agg(
    ivy"com.lihaoyi::utest::0.7.11"
  )

//  def scalacOptions = T{ if (scalaVersion() == "2.12.10") Seq("-opt:l:method") else Nil }
}

object perftests extends Module {
  object bench1 extends PerfTestModule {
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.lihaoyi::scalaparse:1.0.0",
      ivy"com.lihaoyi::pythonparse:1.0.0",
      ivy"com.lihaoyi::cssparse:1.0.0"
    )
  }

  object bench2 extends PerfTestModule {
    def moduleDeps = Seq(
      scalaparse(scala212).jvm.test,
      pythonparse(scala212).jvm.test,
      cssparse(scala212).jvm.test,
      fastparse(scala212).jvm.test
    )
  }

  object compare extends PerfTestModule {
    def moduleDeps = Seq(
      fastparse(scala212).jvm.test,
      scalaparse(scala212).jvm.test,
      pythonparse(scala212).jvm.test
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

  trait PerfTestModule extends ScalaModule with TestModule.Utest {
    def scalaVersion = scala212
    def scalacOptions = Seq("-opt:l:method")
    def resources = T.sources {
      Seq(PathRef(perftests.millSourcePath / "resources")) ++
        fastparse(scala212).jvm.test.resources()
    }
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.11",
      ivy"org.scala-lang:scala-compiler:${scalaVersion()}"
    )
  }
}

object demo extends ScalaJSModule {
  def scalaJSVersion = scalaJS1
  def scalaVersion = scala213
  def moduleDeps = Seq(
    scalaparse(scala213).js(scalaJS1),
    cssparse(scala213).js(scalaJS1),
    pythonparse(scala213).js(scalaJS1),
    fastparse(scala213).js(scalaJS1).test
  )
  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::0.9.8",
    ivy"com.lihaoyi::scalatags::0.9.3"
  )
}
