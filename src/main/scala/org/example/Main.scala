package org.example

import com.comcast.ip4s._
import io.circe.Encoder
import io.circe.generic.semiauto._
import io.circe.literal._
import org.http4s.EntityEncoder
import org.http4s.HttpRoutes
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server._
import zio._
import zio.interop.catz._

import java.time.ZoneId
import java.time.ZonedDateTime

case class Message(greeting: String, generatedAt: ZonedDateTime)
object Message {
  implicit val enc: Encoder[Message] = deriveEncoder
  implicit val entityEnc: EntityEncoder[Task, Message] = jsonEncoderOf
}

sealed trait Greeter {
  def greet(name: String): Task[Message]
}
object Greeter {
  private class Impl extends Greeter {
    override def greet(name: String): Task[Message] = for {
      localTime <- Clock.localDateTime
      zdt = localTime.atZone(ZoneId.systemDefault())
    } yield Message(greeting = s"Hello there, $name", generatedAt = zdt)
  }

  val live: ZLayer[Any, Nothing, Greeter] =
    ZLayer.succeed(new Impl)

  def greet(name: String): ZIO[Greeter, Throwable, Message] =
    ZIO.serviceWithZIO[Greeter](_.greet(name))
}

final class Api(greeter: Greeter) {
  private val dsl = Http4sDsl[Task]
  import dsl._

  val greetRoutes = HttpRoutes.of[Task] { case GET -> Root / "hello" / name =>
    greeter.greet(name).foldZIO(_ => InternalServerError(), Ok(_))
  }
}

object Main extends CatsApp {
  override def run = {
    val program = for {
      greeterService <- ZIO.service[Greeter]
      httpApp = new Api(greeterService).greetRoutes.orNotFound
      server <- EmberServerBuilder
        .default[Task]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(httpApp)
        .build
        .toManagedZIO
        .useForever
        .exitCode
    } yield server

    program
      .provide(Greeter.live)
  }

}
