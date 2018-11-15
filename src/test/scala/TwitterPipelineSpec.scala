import cats.effect.IO
import fs2.Stream
import io.circe.{Encoder, Json}
import org.scalatest._
import io.circe.generic.auto._
import io.circe.syntax._

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._

class TwitterPipelineSpec extends AsyncFlatSpec with Matchers {

  implicit val ec = global

  implicit val encodeTwtterObject: Encoder[TwitterObject] = {
    case t: Tweet => t.asJson
    case DeleteTweet => Json.obj("delete" -> Json.fromBoolean(true))
    case ParseError => Json.obj("Unknown" -> Json.fromBoolean(false))
  }


  class TestPipeline(in: List[TwitterObject], delay: Option[FiniteDuration]) extends TwitterPipelineImp {
    override def source: Stream[IO, Json] = {
      val s = Stream(in.map(_.asJson): _*)
      delay.fold[Stream[IO, Json]](s)(d => s zipLeft Stream.fixedDelay(d))
    }
  }

  def gen(obj: TwitterObject, num: Int): List[TwitterObject] =
    (for (_ <- 1 to num) yield obj).toList

  val baseTweet = Tweet("id", "this is a tweet", Entities(List.empty, List.empty, None))
  val emojiSmileTweet = baseTweet.copy(text = "Smile \uD83D\uDE00")
  val emojiSmileAndWinkTweet = baseTweet.copy(text = "Smile \uD83D\uDE00 and wink \uD83D\uDE09")

  val parserErrorN = 1
  val baseN = 999
  val deleteN = 70
  val smileOnlyN = 90
  val smileWinkN = 100
  val smailTotalN = smileOnlyN + smileWinkN


  val tweets: List[TwitterObject] =
    ParseError ::
      gen(baseTweet, baseN) :::
      gen(DeleteTweet, deleteN) :::
      gen(emojiSmileTweet, smileOnlyN) :::
      gen(emojiSmileAndWinkTweet, smileWinkN)

  val tweetCount: Int = tweets.size - deleteN - parserErrorN

  behavior of "TwitterPipeline"

  it should "return correct stats with delay" in {

    val testPipeline = new TestPipeline(tweets, Some(5.millis))

    val f = testPipeline.tweetStream.compile.drain.unsafeToFuture()

    for {
      _ <- f
      stats <- testPipeline.snapshot.unsafeToFuture()
    } yield {
      stats.allCount should be(tweets.size)
      stats.deleteCount should be(deleteN)
      stats.tweetCount should be(tweetCount)
      stats.parseErrors should be(parserErrorN)
      stats.percentWithEmojis should be(Fraction(smailTotalN, tweetCount).percentage)
      stats.topEmojis.head should be(NameCount("\uD83D\uDE00", smailTotalN))
      stats.topEmojis(1) should be(NameCount("\uD83D\uDE09", smileWinkN))

    }
  }


  it should "return correct stats without delay" in {

    val testPipeline = new TestPipeline(tweets, None)

    val f = testPipeline.tweetStream.compile.drain.unsafeToFuture()

    for {
      _ <- f
      stats <- testPipeline.snapshot.unsafeToFuture()
    } yield {
      println(stats.ratePerSecond)
      stats.allCount should be(tweets.size)
      stats.deleteCount should be(deleteN)
      stats.tweetCount should be(tweetCount)
      stats.parseErrors should be(parserErrorN)
      stats.percentWithEmojis should be(Fraction(smailTotalN, tweetCount).percentage)
      stats.topEmojis.head should be(NameCount("\uD83D\uDE00", smailTotalN))
      stats.topEmojis(1) should be(NameCount("\uD83D\uDE09", smileWinkN))

    }
  }

}