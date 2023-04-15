ThisBuild / scalaVersion := "2.13.10"
ThisBuild / scalafixDependencies += Misc.organizeImports

lazy val root = (project in file("."))
  .settings(
    scalacOptions ++= Seq(
      "-encoding",
      "utf8",
      "-Werror",
      "-Wunused",
      "-Xlint:-byname-implicit", // Shapeless: https://github.com/scala/bug/issues/12072
      "-Ymacro-annotations",
    )
  )
  .settings(
    libraryDependencies ++= Seq(
      Zio.core,
      Zio.interopCats,
      Zio.zmanaged,
      Http4s.dsl,
      Http4s.client,
      Http4s.server,
      Http4s.circe,
      Circe.generic,
      Circe.literal,
      Misc.logback,
    )
  )
