name := "websocket-akka-http"

version := "1.0"


scalaVersion := "2.12.11"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.1.3",
  "com.typesafe.akka" %% "akka-stream" % "2.5.14",

  // TEST
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,

"com.typesafe.akka" %% "akka-http-testkit" % "10.1.3" % Test
)