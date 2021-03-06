sbtPlugin := true

name := "sbt-filterresources"

organization := "de.wayofquality.sbt"

licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

version := "0.1.3-SNAPSHOT"

// We only support sbt-1.x
// crossSbtVersions := Seq("0.13.17", "1.2.7")

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-encoding", "UTF-8"
)

