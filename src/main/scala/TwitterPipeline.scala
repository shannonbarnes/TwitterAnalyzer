import cats.effect.IO.timer
import org.http4s._
import org.http4s.client.blaze._
import org.http4s.client.oauth1
import cats.effect._
import fs2.{Chunk, Stream}
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
  private val collectDuration = 1.seconds

  private[this] var currentState: CumulativeState = CumulativeState.empty

  def currentStats: CurrentStats = currentState.toCurrentStats

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


  private def accumulateSink(s: IOStream[Chunk[Json]]): IOStream[Unit] = s map {chunks =>
    currentState = CumulativeState.merge(
      chunks.map(_.as[TwitterObject].fold(_ => ParseError, identity)).toVector,
      currentState)
  }

  private def processStream(queue: Queue[IO, Json]): IOStream[Unit] = {

    val producer = source.to(queue.enqueue)
    val consumer = queue
      .dequeue
      .groupWithin(Int.MaxValue, collectDuration)
      .to(accumulateSink)

    consumer.concurrently(producer)

  }

 }