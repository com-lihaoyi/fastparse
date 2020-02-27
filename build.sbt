lazy val readme = scalatex.ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/lihaoyi/fastparse/tree/master",
  source = "Readme",
  autoResources = List("out.js", "JProfiler.png")
).settings(
  (resources in Compile) += baseDirectory.value/".."/"out"/"demo"/"fullOpt"/"dest"/"out.js",
  scalaVersion := "2.12.10"
)

