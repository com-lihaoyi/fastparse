import mill._
import scalalib._
import scalajslib._
import scalanativelib._
import publish._
import mill.eval.Result
import mill.modules.Jvm.createJar

val crossVersions = Seq("2.12.10", "2.13.1")
val crossJsVersions = Seq(
  "2.12.10" -> "0.6.32", "2.13.1" -> "0.6.32",
  "2.12.10" -> "1.0.0", "2.13.1" -> "1.0.0"
)
val crossNativeVersions = Seq(
  "2.11.12" -> "0.3.9",
  "2.11.12" -> "0.4.0-M2"
)

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

  object js extends Cross[fastparseJsModule](crossJsVersions:_*)
  class fastparseJsModule(val crossScalaVersion: String, crossScalaJsVersion: String) extends FastparseModule with ScalaJSModule {
    def platformSegment = "js"
    def millSourcePath = super.millSourcePath / ammonite.ops.up
    def scalaJSVersion = crossScalaJsVersion
    object test extends Tests with CommonTestModule{
      def platformSegment = "js"
    }
  }

  object native extends Cross[fastparseNativeModule](crossNativeVersions:_*)
  class fastparseNativeModule(val crossScalaVersion: String, crossScalaNativeVersion: String) extends FastparseModule with ScalaNativeModule {
    def platformSegment = "native"
    def millSourcePath = super.millSourcePath / ammonite.ops.up
    def scalaNativeVersion = crossScalaNativeVersion
    override def docJar = if(crossScalaNativeVersion == "0.3.9") T {
      val outDir = T.dest
      os.makeDir.all(outDir / 'javadoc)
      createJar(Agg(outDir / 'javadoc))(outDir)
    } else super.docJar()
    object test extends Tests with CommonTestModule{
      def platformSegment = "native"
    }
  }
}

trait FastparseModule extends CommonCrossModule{
  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode::0.2.1",
    ivy"com.lihaoyi::geny::0.6.0"
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
  def millSourcePath = super.millSourcePath / ammonite.ops.up
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
  def scalaNativeVersion = "0.3.9"
  def moduleDeps = Seq(fastparse.native())
  object test extends Tests with CommonTestModule{
    def platformSegment = "native"
  }
}


// Remove once mill has proper support for 2.13.0-RC1
object CustomZincWorker extends mill.scalalib.ZincWorkerModule {
  def scalaCompilerBridgeSourceJar(scalaVersion: String, scalaOrganization: String) = {
    val (scalaVersion0, scalaBinaryVersion0, bridgeVersion) = scalaVersion match {
      case s if s.startsWith("2.13.") => ("2.13.0-M2", "2.13.0-M2", "1.2.5")
      case _ => (scalaVersion, mill.scalalib.api.Util.scalaBinaryVersion(scalaVersion), Versions.zinc)
    }

    val (bridgeDep, bridgeName) = {
      val org = "org.scala-sbt"
      val name = "compiler-bridge"
      (ivy"$org::$name:$bridgeVersion", s"${name}_$scalaBinaryVersion0")
    }

    mill.scalalib.Lib.resolveDependencies(
      repositories,
      Lib.depToDependency(_, scalaVersion0, ""),
      Seq(bridgeDep),
      sources = true
    ).map(deps =>
      mill.scalalib.api.Util.grepJar(deps.map(_.path), bridgeName, bridgeVersion, sources = true)
    )
  }
}

trait CommonCrossModule extends CrossScalaModule with PublishModule{
  def zincWorker: ZincWorkerModule =
    CustomZincWorker

  def publishVersion = "2.3.0"
  def artifactName = millModuleSegments.parts.dropRight(2).mkString("-").stripSuffix(s"-$platformSegment")
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
//  def scalacOptions = T{ if (scalaVersion() == "2.12.10") Seq("-opt:l:method") else Nil }

  def platformSegment: String
  def millSourcePath = super.millSourcePath / ammonite.ops.up
  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )


}
trait CommonTestModule extends ScalaModule with TestModule{
  def zincWorker: ZincWorkerModule =
    CustomZincWorker

  def platformSegment: String
  def ivyDeps = Agg(
    ivy"com.lihaoyi::utest::0.7.4",
  )

//  def scalacOptions = T{ if (scalaVersion() == "2.12.10") Seq("-opt:l:method") else Nil }

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
      scalaparse.jvm("2.12.10").test,
      pythonparse.jvm("2.12.10").test,
      cssparse.jvm("2.12.10").test,
      fastparse.jvm("2.12.10").test,
    )

  }


  object compare extends PerfTestModule {
    def moduleDeps = Seq(
      fastparse.jvm("2.12.10").test,
      scalaparse.jvm("2.12.10").test,
      pythonparse.jvm("2.12.10").test
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

  trait PerfTestModule extends ScalaModule with TestModule{
    def scalaVersion = "2.12.10"
    def scalacOptions = Seq("-opt:l:method")
    def resources = T.sources{
      Seq(PathRef(perftests.millSourcePath / "resources")) ++
        fastparse.jvm("2.12.10").test.resources()
    }
    def testFrameworks = Seq("utest.runner.Framework")
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.4",
      ivy"org.scala-lang:scala-compiler:${scalaVersion()}"
    )
  }
}

object demo extends ScalaJSModule{
  def scalaJSVersion = "0.6.32"
  def scalaVersion = "2.13.1"
  def moduleDeps = Seq(
    scalaparse.js("2.13.1", "0.6.32"),
    cssparse.js("2.13.1", "0.6.32"),
    pythonparse.js("2.13.1", "0.6.32"),
    fastparse.js("2.13.1", "0.6.32").test,
  )
  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::0.9.7",
    ivy"com.lihaoyi::scalatags::0.7.0"
  )
}
