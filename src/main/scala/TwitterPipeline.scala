import State._
import cats.effect.IO.timer
import org.http4s._
import org.http4s.client.blaze._
import org.http4s.client.oauth1
import cats.effect._
import fs2.{Pipe, Stream}
import jawnfs2._
import com.typesafe.config.ConfigFactory
import fs2.concurrent.{Queue, SignallingRef}
import io.circe.Json
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global

object TwitterPipeline extends TwitterPipelineImp

trait TwitterPipelineImp {

  implicit val ct = IO.contextShift(ExecutionContext.global)
  implicit val ce = implicitly[ConcurrentEffect[IO]]
  implicit val fc = io.circe.jawn.CirceSupportParser.facade
  implicit val tm = timer(global)

  type IOStream[A] = Stream[IO, A]

  private[this] val conf = ConfigFactory.load

  private val apiKey: String = conf.getString("apiKey")
  private val apiSecretKey: String  = conf.getString("apiSecretKey")
  private val accessToken: String  = conf.getString("accessToken")
  private val accessTokenSecret: String = conf.getString("accessTokenSecret")
  private val req = Request[IO](Method.GET, Uri.uri("https://stream.twitter.com/1.1/statuses/sample.json"))

  private val queue = Queue.unbounded[IO, Json]
  private val state = SignallingRef(emptyCumulativeState).unsafeRunSync()

  def currentStats: IO[StatsSnapshot] = state.get.map(StatsSnapshot.fromState)

  private def authenticate: IO[Request[IO]] =
    oauth1.signRequest(
      req,
      oauth1.Consumer(apiKey, apiSecretKey),
      callback = None,
      verifier = None,
      token = Some(oauth1.Token(accessToken, accessTokenSecret)))

  protected def source: IOStream[Json] =
    for {
      httpClient <- BlazeClientBuilder(global).stream
      oauth      <- Stream.eval(authenticate)
      res        <- httpClient.stream(oauth)
      tweets     <- res.body.chunks.parseJsonStream
    } yield tweets

  def tweetStream: IOStream[Unit] =
     for {
       que  <- Stream.eval(queue)
       str  <- processStream(que)
     } yield str

  def process(take: Int): Pipe[IO, Json, IOStream[Unit]] = in => {
    in.chunkN(take).map { chunks =>

      val processedTweets = IO(
        chunks.map(
          _.as[TwitterObject]
            .fold(_ => ParseError, identity))
          .foldLeft(emptyProcess)(_ combineTweet _)
      )

      Stream.eval(
        processedTweets.flatMap { t =>
          state.update(_.combine(t))
        }
      )
    }
  }

  private def processStream(queue: Queue[IO, Json]): IOStream[Unit] = {

    val timer: IOStream[Unit] = Stream.awakeEvery[IO](1.seconds).evalMap(_ => state.update(_.incSeconds))

    val producer = source
      .to(queue.enqueue)

    val consumer = queue
      .dequeue
      .through(process(10))
      .parJoin(3)

    producer.mergeHaltBoth(consumer).mergeHaltBoth(timer)
  }

 }