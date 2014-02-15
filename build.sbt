name := """bitzguild-stscollection"""

version := "0.1.0"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.10.1",
  "org.specs2" %% "specs2" % "2.3.7" % "test",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "com.google.inject" % "guice" % "3.0",
  "javax.inject" % "javax.inject" % "1",
  "org.mockito" % "mockito-core" % "1.9.5" % "test",
  "org.spire-math" %% "spire" % "0.7.3"
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-encoding", "UTF-8"
  // "-Xcheckinit" // for debugging only, see https://github.com/paulp/scala-faq/wiki/Initialization-Order
  // "-optimise"   // this option will slow your build
)

addCommandAlias("unit", "testOnly unit.*Spec")

addCommandAlias("integration", "testOnly integration.*Spec")
