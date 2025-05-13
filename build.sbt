val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "edi4s",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    javacOptions ++= Seq("-source", "11", "-target", "11"),
    scalacOptions ++= compilerOptions,

    libraryDependencies ++= Seq(
    	"dev.zio" %% "zio" % "2.1.12",
      "dev.zio" %% "zio-nio" % "2.0.2",
      "dev.zio" %% "zio-json" % "0.7.39",
      "co.blocke" %% "scalajack" % "8.1.0",
      "org.apache.poi" % "poi-ooxml" % "5.2.5",
      "com.lihaoyi" %% "pprint" % "0.8.1",
      //"co.blocke" %% "scalajack" % "8.0.2",
      "org.scalameta" %% "munit" % "1.0.0" % Test,

      // Java Junk
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.19.0",
      "com.fasterxml.jackson.module" % "jackson-module-parameter-names" % "2.19.0",
      "com.fasterxml.jackson.datatype" % "jackson-datatype-jdk8" % "2.19.0"
    ),

    Compile / javacOptions += "-parameters"
  )

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-deprecation"
)
