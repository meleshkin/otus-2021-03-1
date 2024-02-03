package me.chuwy.otusfp.home

import cats.data.Validated
import cats.effect._
import fs2.io.file.Files
import io.circe.generic.auto._
import org.http4s.HttpRoutes
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.Router

import scala.concurrent.duration.DurationInt

object Homework {

  case class cnt(counter: Long)

  def homeworkRoutes(ref: Ref[IO, Long]) = {
    val dsl = Http4sDsl[IO]
    import dsl._
    HttpRoutes.of[IO] {

      case GET -> Root / "counter" =>
        ref.getAndUpdate(_ + 1).flatMap(r => Ok(cnt(r)))

      case GET -> Root / "slow" / chunkPath / totalPath / timePath =>
        validatePathVariables(chunkPath, totalPath, timePath) match {
          case Left(_) => BadRequest()
          case Right(value) =>
            val chunk = value._1
            val total = value._2
            val time = value._3
            val source = Files[IO].readAll(java.nio.file.Path.of("someFile.txt"), 1)
              .chunkN(chunk, false)
              .metered(time.second)
              .take(total / chunk)
            Ok(source)
        }
    }
  }

  def validatePathVariables(chunkPath: String, totalPath: String, timePath: String): Either[String, (Int, Int, Int)] = {
    val isIntRegex = "^[1-9]\\d*$"
    val validated = Validated.cond(chunkPath.matches(isIntRegex), 0, "") combine
      Validated.cond(totalPath.matches(isIntRegex), 0, "") combine
      Validated.cond(timePath.matches(isIntRegex), 0, "")

    if (validated.isValid)
      Right(chunkPath.toInt, totalPath.toInt, timePath.toInt)
    else
      Left("Bad request")
  }

  def router(ref: Ref[IO, Long]) = Router("/" -> homeworkRoutes(ref))

  def server(ref: Ref[IO, Long]) = BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.global)
    .bindHttp(8080, "localhost")
    .withHttpApp(router(ref).orNotFound)
}

object CounterServer extends IOApp.Simple {
  override def run: IO[Unit] = {
    val refio: IO[Ref[IO, Long]] = Ref[IO].of(0L)
    refio.flatMap(ref => Homework.server(ref).resource.use(_ => IO.never))
  }
}

