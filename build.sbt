lazy val commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature"),
  version := "0.6.1-SNAPSHOT",
  organization := "uk.rwpenney",
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


def copyVegaConfig(tgtdir: File): Seq[File] = {
  val srcs = Seq("VegaConfig.scala")
  IO.createDirectory(tgtdir)
  IO.copy(srcs.map { s => (root.base / "project" / s, tgtdir / s) }).toSeq
}


lazy val root = (project in file(".")) .
  settings(commonSettings: _*) .
  settings(
    name := "vl4s-generator",
    scalaVersion:= "2.12.4",
    scalacOptions ++= Seq("-deprecation", "-feature"),
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.5.+",
      "org.json4s" %% "json4s-native" % "3.2.11",
      "org.scalatest" %% "scalatest" % "3.0.+" % "test"
    ),
    sourceGenerators in Compile += Def.task {
      copyVegaConfig((sourceManaged in Compile).value)
    }.taskValue
  )


lazy val vl4s = (project in file("vl4s")) .
  settings(commonSettings: _*) .
  settings(
    name := "vl4s",
    scalaVersion := "2.12.4",
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-native" % "3.2.11"
    ),
    sourceGenerators in Compile += Def.task {
      import uk.rwpenney.vl4s.build.VegaConfig
      val autofile = (scalaSource in Compile).value /
                        VegaConfig.defaultAutogenSource
      if (!autofile.exists) {
        println(s"""|WARNING - ${autofile} does not exist
                    | ^  Try running root (vl4s-generator) project to convert
                    | ^  VegaLite schema into Scala source-code"""
                .stripMargin('|'))
      }
      copyVegaConfig((sourceManaged in Compile).value)
    }.taskValue
  )


lazy val `vl4s-demo` = (project in file("vl4s-demo")) .
  dependsOn(vl4s) .
  settings(commonSettings: _*) .
  settings(
    name := "vl4s-demo",
    scalaVersion := "2.12.4",
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.5.+"
    )
  )
