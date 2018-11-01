val Http4sVersion = "0.19.0-M4"
val CirceVersion = "0.10.0"
val Specs2Version = "4.2.0"
val LogbackVersion = "1.2.3"


ThisBuild / scalaVersion := "2.12.7"

lazy val twitter = (project in file("."))
  .settings(
    name := "TwitterAnalyzer",
    scalacOptions ++= Seq("-Ypartial-unification"),
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"      %% "http4s-blaze-client" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
      "io.circe"        %% "circe-generic"       % CirceVersion,
      "org.scalatest"   %% "scalatest"           % "3.0.5" % Test,
      "ch.qos.logback"  %  "logback-classic"     % LogbackVersion,
      "com.typesafe"    % "config"               % "1.3.2",
      "com.vdurmont"    % "emoji-java"           % "4.0.0"
    ),
    addCompilerPlugin("org.spire-math" %% "kind-projector"     % "0.9.6"),
    addCompilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.2.4")
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Ypartial-unification",
)
