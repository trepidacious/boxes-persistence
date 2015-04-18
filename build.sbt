name := "boxes-tc-persistence"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.5"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  "jcenter" at "http://jcenter.bintray.com"
)

libraryDependencies ++= Seq(
  "org.rebeam" %% "transact" % "0.1-SNAPSHOT"
)
