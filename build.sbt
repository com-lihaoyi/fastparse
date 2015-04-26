val shared = Seq(

  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "utest" % "0.3.0"
  ),
  organization := "com.lihaoyi",
  version := "0.1.4",
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
val parsing = project.in(file("parsing")).settings(
  name := "parsing",
  shared
)
val root = project.in(file(".")).settings(
  name := "scala-parser",
  shared
)
