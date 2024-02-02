package me.chuwy.otusfp.home

import cats.effect._
import io.circe.generic.auto._
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.Router

object Counter {

  case class cnt(counter: Long)

  def counterRoutes(ref: Ref[IO, Long]) = HttpRoutes.of[IO] {
    case GET -> Root / "counter" =>
      ref.getAndUpdate(_+1).flatMap(r => Ok(cnt(r)))
  }

  def router(ref: Ref[IO, Long]) = Router("/" -> counterRoutes(ref))

  def server(ref: Ref[IO, Long]) = BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.global)
    .bindHttp(port = 8080, host = "localhost")
    .withHttpApp(router(ref).orNotFound)
}

object CounterServer extends IOApp.Simple {
  override def run: IO[Unit] = {
    val refio: IO[Ref[IO, Long]] = Ref[IO].of(0L)
    refio.flatMap(ref => Counter.server(ref).resource.use(_ => IO.never))
  }
}

