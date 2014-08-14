organization := "me.lessis"

name := "hubcat"

version := "0.2.0-SNAPSHOT"

description := "a vvip client of the github enterprises"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "net.databinder.dispatch" %% "dispatch-json4s-native" % "0.11.2")

// needed for java test options
fork in Test := true

// passing env vars to tests is improved in sbt 0.13.0
javaOptions in Test := Seq("GHUSER", "GHPASS").map(v => "-D%s=%s".format(v, System.getenv(v)))

if (sys.env.getOrElse("TRAVIS", "false").toBoolean) {
  println("using travis")
  seq(ivyLoggingLevel := UpdateLogging.Quiet,
      logLevel in Global := Level.Warn,
      logLevel in Compile := Level.Warn,
      logLevel in Test := Level.Info)
} else seq()

crossScalaVersions := Seq("2.10.4", "2.11.1")

scalaVersion := crossScalaVersions.value.last

scalacOptions := Seq(Opts.compile.deprecation)

licenses := Seq(
  "MIT" ->
  url(s"https://github.com/softprops/${name.value}/blob/${version.value}/LICENSE"))

homepage :=
  Some(url("https://github.com/softprops/%s/" format(name.value)))

publishArtifact in Test := false

publishMavenStyle := true

seq(bintraySettings:_*)

bintray.Keys.packageLabels in bintray.Keys.bintray := Seq("github", "gist")

seq(lsSettings:_*)

LsKeys.tags in LsKeys.lsync := Seq("github", "gist")

seq(buildInfoSettings:_*)

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](version)

buildInfoPackage := "hubcat"
