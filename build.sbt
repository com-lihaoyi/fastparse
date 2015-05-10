publishArtifact := false

publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))

val shared = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
  ) ++ (
    if (scalaVersion.value startsWith "2.11.") Nil
    else Seq(
      compilerPlugin("org.scalamacros" % s"paradise" % "2.0.0" cross CrossVersion.full),
      "org.scalamacros" %% s"quasiquotes" % "2.0.0"
    )
  ),
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "utest" % "0.3.0"
  ),
  scalaJSStage in Global := FullOptStage,
  organization := "com.lihaoyi",
  version := repo.version,
  scalaVersion := "2.11.6",
  libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
  autoCompilerPlugins := true,
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
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
        <url>git://github.com/lihaoyi/scala-parser.git</url>
        <connection>scm:git://github.com/lihaoyi/scala-parser.git</connection>
      </scm>
      <developers>
        <developer>
          <id>lihaoyi</id>
          <name>Li Haoyi</name>
          <url>https://github.com/lihaoyi</url>
        </developer>
      </developers>
)

lazy val utils = crossProject.settings(
  name := "fastparse-utils",
  unmanagedSourceDirectories in Compile ++= {
    if (scalaVersion.value startsWith "2.10.") Seq(baseDirectory.value / ".."/"shared"/"src"/ "main" / "scala-2.10")
    else Seq(baseDirectory.value / ".."/"shared" / "src"/"main" / "scala-2.11")
  }
).settings(shared:_*)
lazy val utilsJS = utils.js
lazy val utilsJVM= utils.jvm


lazy val fastparse = crossProject.dependsOn(utils).settings(
  name := "fastparse"
).settings(shared:_*)
lazy val fastparseJS = fastparse.js
lazy val fastparseJVM = fastparse.jvm
lazy val scalaparse = crossProject.dependsOn(fastparse).settings(
  name := "scalaparse"
).settings(shared:_*)
.jvmSettings(
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test"
)
lazy val scalaparseJS = scalaparse.js
lazy val scalaparseJVM = scalaparse.jvm


lazy val demo = project.enablePlugins(ScalaJSPlugin)
  .dependsOn(fastparseJS % "compile->compile;compile->test", scalaparseJS)
  .settings(shared:_*)
  .settings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0",
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.1",
    emitSourceMaps := false
  )
lazy val readme = scalatex.ScalatexReadme(
  folder = "readme",
  url = "https://github.com/lihaoyi/fastparse/tree/master",
  source = "Readme",
  targetFolder = "target/site",
  autoResources = List("demo-fastopt.js")
).settings(
  (resources in Compile) += {
    (fastOptJS in (demo, Compile)).value
    (artifactPath in (demo,  Compile, fastOptJS)).value

  },
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"repo.scala",
  publishArtifact := false,
  publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
)
