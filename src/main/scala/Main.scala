import cats.effect._
import cats.implicits._
import fs2.concurrent.SignallingRef
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.server.blaze._
import io.circe.syntax._
import io.circe.generic.auto._
import org.http4s.circe._
import State.{CumulativeState, _}


object Main extends IOApp {

  private def statServer(twitterPipeline: TwitterPipeline) = HttpRoutes.of[IO] {
    case GET -> Root / "stats" =>
      Ok(twitterPipeline.snapshot.map(_.asJson))
  }.orNotFound

  def run(args: List[String]): IO[ExitCode] = {

    for {
      state <- SignallingRef[IO, CumulativeState](emptyCumulativeState)
      twitter = new TwitterPipeline(state)
      server = BlazeServerBuilder[IO]
        .bindHttp(8080, "localhost")
        .withHttpApp(statServer(twitter))
        .serve
      result <- twitter.tweetStream
        .merge(server)
        .compile
        .drain
        .as(ExitCode.Success)

    } yield result

  }
}