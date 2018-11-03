import org.http4s._
import org.http4s.client.blaze._
import org.http4s.client.oauth1
import cats.effect._
import fs2.{Chunk, Pure, Sink, Stream}
import jawnfs2._
import com.typesafe.config.ConfigFactory
import fs2.concurrent.Queue

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global

case class State(count: Int)


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

  private val emptyState = State(0)

  implicit val ex = ExecutionContext.global

  val queue = Queue.bounded[IO,State](1)
  val queue2 = Queue.circularBuffer[IO,State](4)

  var currentState: State = emptyState

  def outputSink(s: Stream[IO, State]): Stream[IO, Unit] = s map (state => currentState = state)

  def output2: IO[State] = queue2.flatMap { q =>
    println("I am here!!")
    println(q)
    q.enqueue1(emptyState)
    val r = q.dequeue1
    println("and here")
    val f = r.unsafeToFuture()
    f.onComplete{case _ => "this worked"}
    r
  }

  def output: Stream[IO, State] = Stream.eval(queue2).flatMap { r =>
    r.dequeue
  }.head


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
     val stream = for {
       q <- Stream.eval(queue)
       q2 <- Stream.eval(queue2)
       ts  <- tweetStream2(q, q2)
     } yield ts

    stream
  }

  def tweetStream2(queue: Queue[IO, State], queue2: Queue[IO, State]): Stream[IO, Unit] = {

      val stateStream =  Stream.eval(IO(emptyState)) ++ Stream.repeatEval(queue.dequeue1)

      source
        .groupWithin(Int.MaxValue, collectDuration)
        .zip(stateStream)
        .map {
         case (chunk, s) =>
           println(s)
           s.copy(count = s.count + 1)
        }
        .broadcastTo(queue.enqueue, outputSink)
    }
}

