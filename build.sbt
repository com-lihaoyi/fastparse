
val root = project.in(file(".")).settings(
  name := "scala-parser",
  organization := "com.lihaoyi",
  version := "0.1.0",
  scalaVersion := "2.11.4",
  libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
  autoCompilerPlugins := true,
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "utest" % "0.3.0",
    "org.parboiled" %% "parboiled" % "2.1.0"
  ),
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  testFrameworks += new TestFramework("utest.runner.Framework")
)
