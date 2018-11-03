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

  val twitter = (new TwitterPipeline)

  val statServer = HttpRoutes.of[IO] {
    case GET -> Root / "stats" =>
      Ok(currentStats.asJson)

    case GET -> Root / "stats2" =>
      Ok(twitter.currentState.asJson)

  }.orNotFound



  def run(args: List[String]): IO[ExitCode] = {

    val twitters = twitter.tweetStream

    val server = BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(statServer)
      .serve

    twitters.merge(server)
      .compile
      .drain
      .as(ExitCode.Success)

  }

}