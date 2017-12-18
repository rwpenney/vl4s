lazy val commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature"),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <licenses>
      <license>
        <name>MPL-2.0</name>
        <url>https://www.mozilla.org/en-US/MPL/2.0/</url>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:rwpenney/vl4s.git</url>
      <connection>scm:git:git@github.com:rwpenney/vl4s.git</connection>
    </scm>
  )
)


lazy val root = (project in file(".")) .
  settings(commonSettings: _*) .
  settings(
    name := "vl4s-generator",
    version := "0.2.2-SNAPSHOT",
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
