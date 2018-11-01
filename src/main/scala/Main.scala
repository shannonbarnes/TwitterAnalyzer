import cats.effect._
import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.server.blaze._
import io.circe.syntax._
import io.circe.generic.auto._
import org.http4s.circe._

import InMemoryDataStore.currentStats


object Main extends IOApp {

  val statServer = HttpRoutes.of[IO] {
    case GET -> Root / "stats" =>
      Ok(currentStats.asJson)
  }.orNotFound



  def run(args: List[String]): IO[ExitCode] = {

    (new TwitterPipeline)
      .tweetStream
      .compile
      .drain
      .unsafeRunAsyncAndForget()

    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(statServer)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

  }

}