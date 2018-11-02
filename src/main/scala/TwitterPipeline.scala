import org.http4s._
import org.http4s.client.blaze._
import org.http4s.client.oauth1
import cats.effect._
import fs2.Stream
import jawnfs2._
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global

class TwitterPipeline(implicit t: Timer[IO]) {

  implicit val ctx = IO.contextShift(ExecutionContext.global)
  implicit val ce = implicitly[ConcurrentEffect[IO]]
  implicit val f = io.circe.jawn.CirceSupportParser.facade
  import TwitterObject._

  private[this] val conf = ConfigFactory.load

  private val apiKey: String = conf.getString("apiKey")
  private val apiSecretKey: String  = conf.getString("apiSecretKey")
  private val accessToken: String  = conf.getString("accessToken")
  private val accessTokenSecret: String = conf.getString("accessTokenSecret")

  private val req = Request[IO](Method.GET, Uri.uri("https://stream.twitter.com/1.1/statuses/sample.json"))
  private val collectDuration = 1.seconds

  private def authenticate: IO[Request[IO]] = {
    oauth1.signRequest(
      req,
      oauth1.Consumer(apiKey, apiSecretKey),
      callback = None,
      verifier = None,
      token = Some(oauth1.Token(accessToken, accessTokenSecret)))
  }

  protected def source: Stream[IO, TwitterObject] = {
    val source = for {
      httpClient <- BlazeClientBuilder(global).stream
      oauth      <- Stream.eval(authenticate)
      res        <- httpClient.stream(oauth)
      tweets     <- res.body.chunks.parseJsonStream.map(_.as[TwitterObject])
    } yield tweets

    source
      .map {
        case Right(tweet) => tweet
        case Left(_) => ParseError
      }
  }

  def tweetStream: Stream[IO, Unit] = {
    source
      .groupWithin(Int.MaxValue, collectDuration)
      .to(InMemoryDataStore)
  }

}

