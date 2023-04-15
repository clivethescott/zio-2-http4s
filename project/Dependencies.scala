import sbt._

object Zio {
  private val zioVersion = "2.0.12"
  private val zioInteropCatsVersion = "23.0.0.4"
  private val zmanagedVersion = "2.0.12"

  val core = "dev.zio" %% "zio" % zioVersion
  val interopCats = "dev.zio" %% "zio-interop-cats" % zioInteropCatsVersion
  // https://zio.dev/guides/migrate/zio-2.x-migration-guide#zmanaged
  val zmanaged = "dev.zio" %% "zio-managed" % zmanagedVersion
}

object Http4s {
  private val http4sVersion = "0.23.18"

  val dsl = "org.http4s" %% "http4s-dsl" % http4sVersion
  val server = "org.http4s" %% "http4s-ember-server" % http4sVersion
  val client = "org.http4s" %% "http4s-ember-client" % http4sVersion
  val circe = "org.http4s" %% "http4s-circe" % http4sVersion
}

object Circe {
  private val circeVersion = "0.14.4"

  val generic = "io.circe" %% "circe-generic" % circeVersion
  val literal = "io.circe" %% "circe-literal" % circeVersion
}

object Misc {
  val organizeImports = "com.github.liancheng" %% "organize-imports" % "0.6.0"
  val logback = "ch.qos.logback" % "logback-classic" % "1.4.5"
}
