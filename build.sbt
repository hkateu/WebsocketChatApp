val Http4sVersion = "0.23.23"
val CirceVersion = "0.14.6"
val MunitVersion = "0.7.29"
val LogbackVersion = "1.4.11"
val MunitCatsEffectVersion = "1.0.7"
val CatsParseVersion = "0.3.9"

lazy val root = (project in file("."))
  .settings(
    organization := "rockthejvm",
    name := "websockets",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "3.3.0",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-ember-server" % Http4sVersion,
      "org.http4s"      %% "http4s-ember-client" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "io.circe"        %% "circe-generic"       % CirceVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
      "org.typelevel"   %% "cats-parse"          % CatsParseVersion,
      "org.scalameta"   %% "munit"               % MunitVersion           % Test,
      "org.typelevel"   %% "munit-cats-effect-3" % MunitCatsEffectVersion % Test,
      "ch.qos.logback"  %  "logback-classic"     % LogbackVersion,
    )
  )
