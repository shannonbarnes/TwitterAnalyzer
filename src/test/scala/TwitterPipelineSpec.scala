import cats.effect.IO
import cats.effect.IO.timer
import fs2.Stream
import org.scalatest._
import scala.concurrent.ExecutionContext.global
import scala.concurrent.Future

class TwitterPipelineSpec extends AsyncFlatSpec with Matchers {

  implicit val t = timer(global)
  implicit val ec = global

  class TestPipeline(in: List[TwitterObject]) extends TwitterPipelineImp {
    override def source: Stream[IO, TwitterObject] = Stream(in :_*)
    def toFuture: Future[Unit] = tweetStream.compile.drain.unsafeToFuture()
  }

  def gen(obj: TwitterObject, num: Int): List[TwitterObject] =
    (for (_ <- 1 to num) yield obj).toList

  behavior of "TwitterPipeline"

  it should "return correct stats" in {

    val baseTweet = Tweet("id", "this is a tweet", Entities(List.empty, List.empty, None))
    val emojiSmileTweet = baseTweet.copy(text = "Smile \uD83D\uDE00")
    val emojiSmileAndWinkTweet = baseTweet.copy(text = "Smile \uD83D\uDE00 and wink \uD83D\uDE09")

    val parserErrorN = 1
    val baseN = 100
    val deleteN = 7
    val smileOnlyN = 9
    val smileWinkN = 10
    val smailTotalN = smileOnlyN + smileWinkN


    val tweets: List[TwitterObject] =
      ParseError ::
      gen(baseTweet, baseN) :::
      gen(DeleteTweet, deleteN) :::
      gen(emojiSmileTweet, smileOnlyN) :::
      gen(emojiSmileAndWinkTweet, smileWinkN)

    val tweetCount = tweets.size - deleteN - parserErrorN

    val testPipeline = new TestPipeline(tweets)

    testPipeline.toFuture.map { _ =>
      val stats = testPipeline.currentState.toCurrentStats
      stats.allCount should be (tweets.size)
      stats.deleteCount should be (deleteN)
      stats.tweetCount should be (tweetCount)
      stats.parseErrors should be (parserErrorN)
      stats.percentWithEmojis should be (Fraction(smailTotalN, tweetCount).percentage)
      stats.topEmojis.head should be (NameCount("\uD83D\uDE00", smailTotalN))
      stats.topEmojis(1) should be (NameCount("\uD83D\uDE09", smileWinkN))
    }

  }

}