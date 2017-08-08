import sbtcrossproject.{crossProject, CrossType}
import sbt.Keys._

publishArtifact := false

publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))

lazy val scala210 = "2.10.6"
lazy val scala211 = "2.11.11"
lazy val scala212 = "2.12.3"

crossScalaVersions := Seq(scala210, scala211, scala212)

lazy val nativeSettings = Seq(
  scalaVersion := scala211,
  crossScalaVersions := Seq(scala211)
)

scalaJSUseRhino in Global := false

def macroDependencies(version: String) =
  Seq(
    "org.scala-lang" % "scala-reflect" % version % "provided",
    "org.scala-lang" % "scala-compiler" % version % "provided"
  ) ++
  (if (version startsWith "2.10.")
     Seq(compilerPlugin("org.scalamacros" % s"paradise" % "2.1.0" cross CrossVersion.full),
         "org.scalamacros" %% s"quasiquotes" % "2.1.0")
   else
     Seq())

val shared = Seq(
  libraryDependencies ++= macroDependencies(scalaVersion.value),
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "utest" % "0.4.8" % "test",
    "com.lihaoyi" %%% "sourcecode" % "0.1.4"
  ),
  scalaJSStage in Global := FullOptStage,
  organization := "com.lihaoyi",
  version := _root_.fastparse.Constants.version,
  scalaVersion := scala212,
  libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.5" % "provided",
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.5"),
  autoCompilerPlugins := true,
  testFrameworks += new TestFramework("utest.runner.Framework"),
  publishTo := Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
  pomExtra :=
    <url>https://github.com/lihaoyi/scala-parser</url>
      <licenses>
        <license>
          <name>MIT license</name>
          <url>http://www.opensource.org/licenses/mit-license.php</url>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/lihaoyi/fastparse.git</url>
        <connection>scm:git://github.com/lihaoyi/fastparse.git</connection>
      </scm>
      <developers>
        <developer>
          <id>lihaoyi</id>
          <name>Li Haoyi</name>
          <url>https://github.com/lihaoyi</url>
        </developer>
      </developers>
)

lazy val utils = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .settings(shared:_*)
  .settings(
    name := "fastparse-utils",
    unmanagedSourceDirectories in Compile ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n >= 12 =>
          Seq(baseDirectory.value / ".." / "shared" / "src" / "main" / "scala-2.11")
        case _ =>
          Seq()
      }
    }
  )
  .nativeSettings(nativeSettings)
lazy val utilsJS = utils.js
lazy val utilsJVM= utils.jvm
lazy val utilsNative = utils.native


lazy val fastparse = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .dependsOn(utils)
  .settings(shared:_*)
  .settings(
    name := "fastparse",
    sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
      val file = dir/"fastparse"/"core"/"SequencerGen.scala"
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
          package fastparse.core
          trait SequencerGen[Sequencer[_, _, _]] extends LowestPriSequencer[Sequencer]{
            protected[this] def Sequencer0[A, B, C](f: (A, B) => C): Sequencer[A, B, C]
            ${tuples.mkString("\n")}
          }
          trait LowestPriSequencer[Sequencer[_, _, _]]{
            protected[this] def Sequencer0[A, B, C](f: (A, B) => C): Sequencer[A, B, C]
            implicit def Sequencer1[T1, T2]: Sequencer[T1, T2, (T1, T2)] = Sequencer0{case (t1, t2) => (t1, t2)}
          }
        """.stripMargin
      IO.write(file, output)
      Seq(file)
    }
  )
  // In order to make the midi-parser-test in fastparseJVM/test:run work
  .jvmSettings(fork in (Test, run) := true)
  .nativeSettings(nativeSettings)

lazy val fastparseJS = fastparse.js
lazy val fastparseJVM = fastparse.jvm
lazy val fastparseNative = fastparse.native

lazy val fastparseByte = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .dependsOn(fastparse)
  .settings(shared:_*)
  .settings(
    name := "fastparse-byte",
    libraryDependencies += "org.scodec" %%% "scodec-bits" % "1.1.5"
  )

lazy val fastparseByteJS = fastparseByte.js
lazy val fastparseByteJVM = fastparseByte.jvm
lazy val fastparseByteNative = fastparseByte.native
// Native support waiting for release of https://github.com/scodec/scodec-bits/pull/72

lazy val scalaparse = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .dependsOn(fastparse)
  .settings(shared:_*)
  .settings(
    name := "scalaparse"
    )
  .jvmSettings(
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test"
  )
  .nativeSettings(nativeSettings)

lazy val scalaparseJS = scalaparse.js

lazy val scalaparseJVM = scalaparse.jvm

lazy val scalaparseNative = scalaparse.native


lazy val pythonparse = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .dependsOn(fastparse)
  .settings(shared:_*)
  .settings(
    name := "pythonparse"
  )
  .nativeSettings(nativeSettings)


lazy val pythonparseJVM = pythonparse.jvm
lazy val pythonparseJS = pythonparse.js
lazy val pythonparseNative = pythonparse.native


lazy val cssparse = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .dependsOn(fastparse)
  .settings(name := "cssparse")
  .settings(shared:_*)
  .jvmSettings(
    libraryDependencies += "net.sourceforge.cssparser" % "cssparser" % "0.9.18" % "test"
  )
  .nativeSettings(nativeSettings)

lazy val cssparseJVM = cssparse.jvm
lazy val cssparseJS = cssparse.js
lazy val cssparseNative = cssparse.native

lazy val classparse = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .dependsOn(fastparseByte)
  .settings(shared:_*)
  .settings(
    name := "classparse"
  )

lazy val classparseJVM = classparse.jvm
lazy val classparseJS = classparse.js
lazy val classparseNative = classparse.native
// Native support waiting for release of https://github.com/scodec/scodec-bits/pull/72

lazy val perftests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .dependsOn(
    fastparse % "compile->compile;compile->test",
    fastparseByte % "compile->compile;compile->test",
    pythonparse,
    scalaparse,
    cssparse,
    classparse
  )
  .settings(shared:_*)
  .settings(
    name := "perfomance-tests",
    parallelExecution := false
  )

lazy val perftestsJVM = perftests.jvm
lazy val perftestsJS = perftests.js
lazy val perftestsNative = perftests.native
// Native support waiting for release of https://github.com/scodec/scodec-bits/pull/72

lazy val modules = project
  .aggregate(
    fastparseJS,
    fastparseJVM,
    fastparseNative,
    fastparseByteJS,
    fastparseByteJVM,
    pythonparseJS,
    pythonparseJVM,
    pythonparseNative,
    cssparseJS,
    cssparseJVM,
    cssparseNative,
    scalaparseJS,
    scalaparseJVM,
    scalaparseNative,
    classparseJVM,
    classparseJS,
    utilsJS,
    utilsJVM,
    utilsNative
  )
  .settings(
    publishArtifact := false,
    publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
  )

lazy val demo = project.enablePlugins(ScalaJSPlugin)
  .dependsOn(
    fastparseJS % "compile->compile;compile->test",
    fastparseByteJS % "compile->compile;compile->test",
    scalaparseJS,
    cssparseJS,
    classparseJS
  )
  .settings(shared:_*)
  .settings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2",
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.5",
    emitSourceMaps := false,
    publishArtifact := false,
    publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
  )
lazy val readme = scalatex.ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/lihaoyi/fastparse/tree/master",
  source = "Readme",
  autoResources = List("demo-opt.js")
).settings(
  (resources in Compile) += {
    (fullOptJS in (demo, Compile)).value
    (artifactPath in (demo,  Compile, fullOptJS )).value
  },
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala",
  publishArtifact := false,
  publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
)
