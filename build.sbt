val macros = project.in(file("macros")).settings(
  scalaVersion := "2.11.4",
  libraryDependencies ++= Seq(
    "org.parboiled" %% "parboiled" % "2.0.1",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
  )
)

val root = project.in(file(".")).dependsOn(macros).settings(
  scalaVersion := "2.11.4",
  libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
  autoCompilerPlugins := true,
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "utest" % "0.2.4",
    "org.parboiled" %% "parboiled" % "2.0.1"
  ),
  testFrameworks += new TestFramework("utest.runner.JvmFramework")
)
