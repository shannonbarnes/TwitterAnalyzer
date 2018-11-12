import java.net.URL

import State.ProcessedTweets
import com.vdurmont.emoji.EmojiParser

import scala.collection.JavaConverters._
import scala.util.Try

trait TweetProcessor extends ProcessedTweets { self: Tweet =>

  val deleteCount: Int = 0
  val parseErrors: Int = 0
  val tweetCount: Int = 1

  val hashtags: List[String] = entities.hashtags.map(_.text)

  val domains: List[String] =
    for {
      urlObj <- entities.urls
      url    = urlObj.unwound.fold(urlObj.expanded_url)(_.url)
      u      <- Try {new URL(url)}.toOption
    } yield u.getHost

  val emojis: List[String] = EmojiParser.extractEmojis(text).asScala.toList

  val containedEmojiCount: Int = boolToInt(emojis.nonEmpty)
  val containedUrlCount: Int = boolToInt(domains.nonEmpty)
  val containsPhotoCount: Int = boolToInt(containsPhoto)

  private def boolToInt(b: Boolean): Int = if (b) 1 else 0
  private def containsPhoto: Boolean =
    entities.media.exists(_.exists(_.`type` == "photo")) ||
      domains.exists(_.contains("pic.twitter.com")) ||
      domains.exists(_.contains("instagram"))
}

