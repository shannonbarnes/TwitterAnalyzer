import cats.effect.IO
import cats.effect.IO.timer
import fs2.Stream
import org.scalatest._
import scala.concurrent.ExecutionContext.global
import scala.concurrent.Future
import FractionSyntax._

class TwitterPipelineSpec extends AsyncFlatSpec with Matchers {

  implicit val t = timer(global)
  implicit val ec = global

  class TestPipeline(val in: Stream[IO, TwitterObject]) extends TwitterPipeline {
    override def source: Stream[IO, TwitterObject] = in
    def toFuture: Future[Unit] = tweetStream.compile.drain.unsafeToFuture()
  }

  def test(in: List[TwitterObject]): Future[Unit] = {
    new TestPipeline(Stream(in :_*)).toFuture
  }

  def gen(obj: TwitterObject, num: Int): List[TwitterObject] =
    (for (_ <- 1 to num) yield obj).toList


  behavior of "TwitterPipeline"

  it should "return correct stats" in {

    val baseTweet = Tweet("id", "this is a tweet", Entities(List.empty, List.empty, None))
    val emojiSmileTweet = baseTweet.copy(text = "Smile \uD83D\uDE00")
    val emojiSmileAndWinkTweet = baseTweet.copy(text = "Smile \uD83D\uDE00 and wink \uD83D\uDE09")

    val tweets: List[TwitterObject] =
      ParseError ::
      gen(baseTweet, 100) :::
      gen(DeleteTweet, 10) :::
      gen(emojiSmileTweet, 10) :::
      gen(emojiSmileAndWinkTweet, 10)

    val tweetCount = tweets.size - 10 - 1

    val f = test(tweets)

    f.map { _ =>
      val stats = InMemoryDataStore.currentStats
      stats.allCount should be (tweets.size)
      stats.deleteCount should be (10)
      stats.tweetCount should be (tweetCount)
      stats.parseErrors should be (1)
      stats.percentWithEmojis should be (Fraction(20, tweetCount).percentage)
      stats.topEmojis.head should be (NameCount("\uD83D\uDE00", 20))
      stats.topEmojis(1) should be (NameCount("\uD83D\uDE09", 10))
    }

  }



}