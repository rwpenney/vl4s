lazy val commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature")
)


lazy val root = (project in file(".")) .
  settings(commonSettings: _*) .
  settings(
    name := "vl4s-generator",
    version := "0.2.1-SNAPSHOT",
    scalaVersion:= "2.12.4",
    scalacOptions ++= Seq("-deprecation", "-feature"),
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.5.+",
      "org.json4s" %% "json4s-native" % "3.2.11",
      "org.scalatest" %% "scalatest" % "3.0.+" % "test"
    )
  )


lazy val vl4s = (project in file("vl4s")) .
  dependsOn(root) .
  aggregate(root) .
  settings(commonSettings: _*) .
  settings(
    name := "vl4s",
    scalaVersion := "2.12.4"
  )
