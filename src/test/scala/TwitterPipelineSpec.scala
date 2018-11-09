import cats.effect.IO
import fs2.Stream
import io.circe.{Encoder, Json}
import org.scalatest._
import io.circe.generic.auto._
import io.circe.syntax._

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._

class TwitterPipelineSpec extends FlatSpec with Matchers {

  implicit val ec = global

  implicit val encodeTwtterObject: Encoder[TwitterObject] = {
      case t: Tweet => t.asJson
      case DeleteTweet => Json.obj("delete" -> Json.fromBoolean(true))
      case ParseError => Json.obj("Unknown" -> Json.fromBoolean(false))
    }


  class TestPipeline(in: List[TwitterObject], delay: FiniteDuration) extends TwitterPipelineImp {
    override def source: Stream[IO, Json] = Stream(in.map(_.asJson) :_*) zipLeft Stream.fixedDelay(delay)
  }

  def gen(obj: TwitterObject, num: Int): List[TwitterObject] =
    (for (_ <- 1 to num) yield obj).toList

  behavior of "TwitterPipeline"

  it should "return correct stats" in {

    val baseTweet = Tweet("id", "this is a tweet", Entities(List.empty, List.empty, None))
    val emojiSmileTweet = baseTweet.copy(text = "Smile \uD83D\uDE00")
    val emojiSmileAndWinkTweet = baseTweet.copy(text = "Smile \uD83D\uDE00 and wink \uD83D\uDE09")

    val parserErrorN = 1
    val baseN = 1000
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

    val tweetCount = tweets.size - deleteN - parserErrorN

    val testPipeline = new TestPipeline(tweets, 5.millis)

    testPipeline.tweetStream.compile.drain.unsafeRunAsyncAndForget()
    Thread.sleep(1000 * 10)
    val stats = testPipeline.currentStats
    stats.allCount should be (tweets.size)
    stats.deleteCount should be (deleteN)
    stats.tweetCount should be (tweetCount)
    stats.parseErrors should be (parserErrorN)
    stats.percentWithEmojis should be (Fraction(smailTotalN, tweetCount).percentage)
    stats.topEmojis.head should be (NameCount("\uD83D\uDE00", smailTotalN))
    stats.topEmojis(1) should be (NameCount("\uD83D\uDE09", smileWinkN))

  }

}