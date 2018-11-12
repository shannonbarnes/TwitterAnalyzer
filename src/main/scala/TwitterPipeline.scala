import State._
import cats.effect.IO.timer
import org.http4s._
import org.http4s.client.blaze._
import org.http4s.client.oauth1
import cats.effect._
import fs2.{Pipe, Stream}
import jawnfs2._
import com.typesafe.config.ConfigFactory
import fs2.concurrent.Queue
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

  import TwitterObject._

  private[this] val conf = ConfigFactory.load

  private val apiKey: String = conf.getString("apiKey")
  private val apiSecretKey: String  = conf.getString("apiSecretKey")
  private val accessToken: String  = conf.getString("accessToken")
  private val accessTokenSecret: String = conf.getString("accessTokenSecret")
  private val req = Request[IO](Method.GET, Uri.uri("https://stream.twitter.com/1.1/statuses/sample.json"))

  private[this] var currentState = State.emptyCumulativeState

  def currentStats: CurrentStats = CurrentStats.fromState(currentState)

  private val queue = Queue.unbounded[IO, Json]

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
       q  <- Stream.eval(queue)
       ts <- processStream(q)
     } yield ts


  private def accumulateSink(s: IOStream[Either[ProcessedTweets, FiniteDuration]]): IOStream[Unit] = s map {
    case Left(state) => currentState = currentState.combine(state)
    case Right(_) => TimeRate.insertSecond(currentState.all)
  }


  def log[A](prefix : String) : Pipe[IO,A,A] = { in =>
    in.evalMap(i => IO { println(i) ; i} )
  }

  def process(take: Int): Pipe[IO, Json, IOStream[ProcessedTweets]] = in => {
    in.chunkN(take).map { chunks =>
      Stream.eval(
        IO(
          chunks.map(_.as[TwitterObject].fold(_ => ParseError, identity)).foldLeft(State.emptyProcess)(_ combine _)
        )
      )
    }
  }


  private def processStream(queue: Queue[IO, Json]): IOStream[Unit] = {

    val timer = Stream.awakeEvery[IO](1.seconds)

    val producer = source
      .to(queue.enqueue)

    val consumer = queue
      .dequeue
      .through(process(10))
      .parJoin(3)
      .either(timer)
      .to(accumulateSink)

    consumer.concurrently(producer)

  }

 }