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


  //private val stateStream = Stream.eval(IO(emptyState))


  //val queue = Queue.synchronous[IO,State]

  //def queueStream = Stream.eval(queue)
  val queue = Queue.bounded[IO,State](1)

  //def fromQueue: IO[State] = {
  //  queue.flatMap{ q =>
  //    println("from queue")
  //    val v = q.dequeue1
  //    println("dequeed")
  //    v
  //  }
  //}

  //def stateStream: Stream[IO, State] = Stream.eval(IO(emptyState)) ++ Stream.repeatEval(fromQueue)

  //def stateSink(s: Stream[IO, State]): Stream[IO, Unit] = {
  //  Stream.eval(queue).map(q => s.to(q.enqueue))
  //  s.map { newState => println("new state")}
    //s.to()
    //println("am I ever here!!!!")
    //s.map { newState =>
      //println("OR here!!!")
      //println(newState)
      //queue.map { q =>
        //println("new State inserted")
        //q.enqueue1(newState)
      //}
    //}



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
       //e <- Stream.eval(q.enqueue1(emptyState))
       ts  <- tweetStream2(q)
     } yield ts

    stream
  }

  def tweetStream2(queue: Queue[IO, State]): Stream[IO, Unit] = {

      val stateStream =  Stream.eval(IO(emptyState)) ++ Stream.repeatEval(queue.dequeue1)
      //val stateStream =  Stream.eval(IO(emptyState)).repeat

      //queue.
      //val stateStream =  Stream.eval(queue.dequeue1)

      source
        .groupWithin(Int.MaxValue, collectDuration)
        .zip(stateStream)
        .map {
         case (chunk, s) =>
           println(s)
           s.copy(count = s.count + 1)
        }
        .to(queue.enqueue)
    }
}

