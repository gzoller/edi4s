val scala3Version = "3.6.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "edi4s",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
    	"dev.zio" %% "zio" % "2.1.12",
      "dev.zio" %% "zio-nio" % "2.0.2",
      "dev.zio" %% "zio-json" % "0.7.39",
      "co.blocke" %% "scalajack" % "orderedMap_af1bda",
      //"co.blocke" %% "scalajack" % "8.0.2",
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )
