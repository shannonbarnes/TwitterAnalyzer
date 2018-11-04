import cats.effect.IO.timer
import org.http4s._
import org.http4s.client.blaze._
import org.http4s.client.oauth1
import cats.effect._
import fs2.{Chunk, Stream}
import jawnfs2._
import com.typesafe.config.ConfigFactory
import fs2.concurrent.Queue

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global


object TwitterPipeline extends TwitterPipelineImp

trait TwitterPipelineImp {

  implicit val ctx = IO.contextShift(ExecutionContext.global)
  implicit val ce = implicitly[ConcurrentEffect[IO]]
  implicit val f = io.circe.jawn.CirceSupportParser.facade
  implicit val t = timer(global)

  import TwitterObject._

  private[this] val conf = ConfigFactory.load

  private val apiKey: String = conf.getString("apiKey")
  private val apiSecretKey: String  = conf.getString("apiSecretKey")
  private val accessToken: String  = conf.getString("accessToken")
  private val accessTokenSecret: String = conf.getString("accessTokenSecret")

  private val req = Request[IO](Method.GET, Uri.uri("https://stream.twitter.com/1.1/statuses/sample.json"))
  private val collectDuration = 1.seconds

  var currentState: CumulativeState = CumulativeState.empty

  private val queue = Queue.bounded[IO,CumulativeState](1)

  private def outputSink(s: Stream[IO, CumulativeState]): Stream[IO, Unit] = s map (state => currentState = state)

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
     for {
       q  <- Stream.eval(queue)
       ts <- processStream(q)
     } yield ts
  }

  private def accumulate(f: Stream[IO, (Chunk[TwitterObject], CumulativeState)]): Stream[IO, CumulativeState] =
    f map {case (c,s) => CumulativeState.append(c.toVector, s)}

  private def processStream(queue: Queue[IO, CumulativeState]): Stream[IO, Unit] = {

    val stateStream =  Stream.eval(IO(CumulativeState.empty)) ++ Stream.repeatEval(queue.dequeue1)

    source
      .groupWithin(Int.MaxValue, collectDuration)
      .zip(stateStream)
      .through(accumulate)
      .broadcastTo(queue.enqueue, outputSink)
    }

 }

