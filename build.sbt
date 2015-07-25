name := "boxes-persistence"

version := "0.1-SNAPSHOT"

organization := "org.rebeam"

scalaVersion := "2.11.6"

resolvers ++= Seq(
  "jcenter" at "http://jcenter.bintray.com",
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.rebeam" %% "boxes-core" % "0.1-SNAPSHOT",
//  "io.spray" %%  "spray-json" % "1.3.1",
//  "com.chuusai" %% "shapeless" % "2.1.0",
  "com.google.protobuf" % "protobuf-java" % "2.6.1",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"  //Note that this is NOT the most recent version of scalacheck,
                                                        //but IS the one referenced by scalatest on github
)

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint"
)

testOptions in Test += Tests.Argument("-oF")