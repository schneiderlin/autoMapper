scalaVersion     := "2.12.8"
name := """scala-auto-mapper"""

val shapelessVersion = "2.3.3"

libraryDependencies +=  "org.scalatest" %% "scalatest" % "3.2.0-SNAP10"
libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % shapelessVersion
)