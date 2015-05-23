name := "boxes-tc-persistence"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  "jcenter" at "http://jcenter.bintray.com",
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.rebeam" %% "transact" % "0.1-SNAPSHOT",
  "io.spray" %%  "spray-json" % "1.3.1",
  "com.chuusai" %% "shapeless" % "2.1.0",
  "com.google.protobuf" % "protobuf-java" % "2.6.1"
)

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Ywarn-unused-import"
)