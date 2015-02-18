organization := "me.lessis"

name := "hubcat"

version := "0.2.0-SNAPSHOT"

description := "a vvip client of the github enterprises"

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "1.8.0",
  "net.databinder.dispatch" %% "dispatch-json4s-native" % "0.11.0",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test")

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

crossScalaVersions := Seq("2.9.3", "2.10.2")

scalaVersion := "2.10.2"

scalacOptions := Seq(Opts.compile.deprecation)

licenses := Seq(
  "MIT" ->
  url("https://github.com/softprops/%s/blob/%s/LICENSE" format(name.value, version.value)))

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
