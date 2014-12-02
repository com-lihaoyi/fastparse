
val root = project.in(file(".")).settings(
  scalaVersion := "2.11.4",
  libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
  autoCompilerPlugins := true,
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "utest" % "0.2.4",
    "org.parboiled" %% "parboiled" % "2.0.2-SNAPSHOT"
  ),
  resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  testFrameworks += new TestFramework("utest.runner.JvmFramework")
)
