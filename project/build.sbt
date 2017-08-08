addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.19")

addSbtPlugin("com.lihaoyi" % "scalatex-sbt-plugin" % "0.3.9")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.9")

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC10")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.0")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.5")

addSbtPlugin("org.scala-native" % "sbt-crossproject" % "0.2.0")

addSbtPlugin("org.scala-native" % "sbt-scalajs-crossproject" % "0.2.0")

// scala-native 0.3.1 causes error in fastparseNative/compile, which is fixed in
// https://github.com/scala-native/scala-native/pull/902
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.2")
