organization := "me.lessis"

name := "hubcat"

version := "0.1.0"

description := "a vvip client of the github enterprises"

libraryDependencies += "net.databinder.dispatch" %% "dispatch-json4s-native" % "0.9.5"

crossScalaVersions :=
      Seq("2.8.2", "2.9.0-1", "2.9.1", "2.9.1-1", "2.9.2")

scalacOptions += Opts.compile.deprecation

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
