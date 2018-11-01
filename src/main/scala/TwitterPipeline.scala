import org.http4s._
import org.http4s.client.blaze._
import org.http4s.client.oauth1
import cats.effect._
import fs2.Stream
import jawnfs2._
import java.util.concurrent.Executors
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

  val apiKey: String = conf.getString("apiKey")
  val apiSecretKey: String  = conf.getString("apiSecretKey")
  val accessToken: String  = conf.getString("accessToken")
  val accessTokenSecret: String = conf.getString("accessTokenSecret")

  private val req = Request[IO](Method.GET, Uri.uri("https://stream.twitter.com/1.1/statuses/sample.json"))
  private val collectDuration = 1.seconds

  def authenticate: IO[Request[IO]] = {
    oauth1.signRequest(
      req,
      oauth1.Consumer(apiKey, apiSecretKey),
      callback = None,
      verifier = None,
      token = Some(oauth1.Token(accessToken, accessTokenSecret)))
  }


  def tweetStream(blockingEC: ExecutionContext): Stream[IO, Unit] = {

    val source = for {
      httpClient <- BlazeClientBuilder(global).stream
      oauth      <- Stream.eval(authenticate)
      res        <- httpClient.stream(oauth)
      tweets     <- res.body.chunks.parseJsonStream.map(_.as[TwitterObject])
    } yield tweets

    source
      .collect{
        case Right(tweet) => tweet
        case Left(_) => ParseError
      }
      .groupWithin(Int.MaxValue, collectDuration)
      .to(InMemoryDataStore)
  }


  def blockingEcStream: Stream[IO, ExecutionContext] =
    Stream.bracket(IO.delay(Executors.newFixedThreadPool(4)))(pool =>
      IO.delay(pool.shutdown()))
      .map(ExecutionContext.fromExecutorService)


  def run: IO[Unit] =
    blockingEcStream.flatMap { blockingEc =>
      tweetStream(blockingEc)
    }.compile.drain

}

