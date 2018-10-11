lazy val readme = scalatex.ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/lihaoyi/fastparse/tree/master",
  source = "Readme",
  autoResources = List("demo-opt.js")
).settings(
  shared,
  (resources in Compile) += {
    (fullOptJS in (demo, Compile)).value
    (artifactPath in (demo,  Compile, fullOptJS )).value
  },
  is212Only,
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala",
  noPublish
)

