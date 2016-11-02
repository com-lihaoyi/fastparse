import sbt.Keys._

publishArtifact := false

publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))

crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.0")

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
    "com.lihaoyi" %%% "utest" % "0.4.4" % "test",
    "com.lihaoyi" %%% "sourcecode" % "0.1.3"
  ),
  scalaJSStage in Global := FullOptStage,
  organization := "com.lihaoyi",
  version := _root_.fastparse.Constants.version,
  scalaVersion := "2.11.8",
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

lazy val utils = crossProject
  .settings(shared:_*)
  .settings(
    name := "fastparse-utils",
    unmanagedSourceDirectories in Compile ++= {
      if (scalaVersion.value startsWith "2.12.") Seq(baseDirectory.value / ".."/"shared"/"src"/ "main" / "scala-2.11")
      else Seq()
    }
  )
lazy val utilsJS = utils.js
lazy val utilsJVM= utils.jvm


lazy val fastparse = crossProject
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

lazy val fastparseJS = fastparse.js
lazy val fastparseJVM = fastparse.jvm

lazy val fastparseByte = crossProject
  .dependsOn(fastparse)
  .settings(shared:_*)
  .settings(
    name := "fastparse-byte",
    libraryDependencies += "org.scodec" %%% "scodec-bits" % "1.1.2"
  )

lazy val fastparseByteJS = fastparseByte.js
lazy val fastparseByteJVM = fastparseByte.jvm

lazy val scalaparse = crossProject
  .dependsOn(fastparse)
  .settings(shared:_*)
  .settings(
    name := "scalaparse"
    )
  .jvmSettings(
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test"
  )

lazy val scalaparseJS = scalaparse.js

lazy val scalaparseJVM = scalaparse.jvm


lazy val pythonparse = crossProject
  .dependsOn(fastparse)
  .settings(shared:_*)
  .settings(
    name := "pythonparse"
  )


lazy val pythonparseJVM = pythonparse.jvm
lazy val pythonparseJS = pythonparse.js

lazy val cssparse = crossProject
  .dependsOn(fastparse)
  .settings(name := "cssparse")
  .settings(shared:_*)
  .jvmSettings(
    libraryDependencies += "net.sourceforge.cssparser" % "cssparser" % "0.9.18"
  )

lazy val cssparseJVM = cssparse.jvm
lazy val cssparseJS = cssparse.js

lazy val classparse = crossProject
  .dependsOn(fastparseByte)
  .settings(shared:_*)
  .settings(
    name := "classparse"
  )

lazy val classparseJVM = classparse.jvm
lazy val classparseJS = classparse.js

lazy val perftests = crossProject
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

lazy val modules = project
  .aggregate(
    fastparseJS,
    fastparseJVM,
    fastparseByteJS,
    fastparseByteJVM,
    pythonparseJS,
    pythonparseJVM,
    cssparseJS,
    cssparseJVM,
    scalaparseJS,
    scalaparseJVM,
    classparseJVM,
    classparseJS,
    utilsJS,
    utilsJVM
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
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.2",
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.3",
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
    (artifactPath in (demo,  Compile, fullOptJS)).value
  },
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala",
  publishArtifact := false,
  publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
)
