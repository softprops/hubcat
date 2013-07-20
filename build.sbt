organization := "me.lessis"

name := "hubcat"

version := "0.1.1-SNAPSHOT"

description := "a vvip client of the github enterprises"

libraryDependencies ++= Seq("net.databinder.dispatch" %% "dispatch-json4s-native" % "0.10.1")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

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

crossScalaVersions := Seq("2.9.3", "2.10.0", "2.10.1", "2.10.2")

scalaVersion := "2.10.0"

scalacOptions <++= (scalaVersion).map { sv =>
  if (sv.startsWith("2.10")) Seq(Opts.compile.deprecation, "-feature") else Seq(Opts.compile.deprecation)
}

publishTo := Some(Opts.resolver.sonatypeStaging)

licenses <<= version(v =>
      Seq("MIT" ->
          url("https://github.com/softprops/hubcat/blob/%s/LICENSE" format v)))

homepage :=
  Some(url("https://github.com/softprops/hubcat/"))

publishArtifact in Test := false

publishMavenStyle := true

pomExtra := (
  <scm>
    <url>git@github.com:softprops/hubcat.git</url>
    <connection>scm:git:git@github.com:softprops/hubcat.git</connection>
  </scm>
  <developers>
    <developer>
      <id>softprops</id>
      <name>Doug Tangren</name>
      <url>http://github.com/softprops</url>
    </developer>
  </developers>)

seq(lsSettings:_*)

LsKeys.tags in LsKeys.lsync := Seq("github", "gist")

seq(buildInfoSettings:_*)

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](version)

buildInfoPackage := "hubcat"
