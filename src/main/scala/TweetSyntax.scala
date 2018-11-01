import java.net.URL

import com.vdurmont.emoji.EmojiParser
import scala.collection.JavaConverters._
import scala.util.Try

object TweetSyntax {

  implicit class tweetExtractor(t: Tweet) {
    def hashTags: List[String] = t.entities.hashtags.map(_.text)

    def emojis: List[String] = EmojiParser.extractEmojis(t.text).asScala.toList

    def domains: List[String] =
      for {
        urlObj <- t.entities.urls
        url    = urlObj.unwound.fold(urlObj.expanded_url)(_.url)
        u      <- Try {new URL(url)}.toOption
      } yield u.getHost

    def containPhoto(domains: List[String]): Boolean =
      t.entities.media.exists(_.exists(_.`type` == "photo")) ||
        domains.exists(_.contains("pic.twitter.com")) ||
          domains.exists(_.contains("instagram"))


  }



}
