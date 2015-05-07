val shared = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value
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
  scalaJSStage in Global := FastOptStage,
  organization := "com.lihaoyi",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.11.6",
  crossScalaVersions := Seq("2.11.6", "2.10.5"),
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
  name := "fastparse-utils"
).settings(shared:_*)
lazy val utilsJS = utils.js
lazy val utilsJVM= utils.jvm


lazy val fastparse = crossProject.dependsOn(utils).settings(
  name := "fastparse"
).settings(shared:_*)
lazy val fastparseJS = fastparse.js
lazy val fastparseJVM = fastparse.jvm
lazy val scalaparser = crossProject.dependsOn(fastparse).settings(
  name := "fastparse-scala"
).settings(shared:_*)
.jvmSettings(
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test"
)
lazy val scalaparserJS = scalaparser.js
lazy val scalaparserJVM = scalaparser.jvm

