import io.circe.{Decoder, FailedCursor}
import io.circe.generic.auto._

object TwitterObject {

  implicit val decodeTwitterObject: Decoder[TwitterObject] = Decoder.instance(c =>
    c.downField("delete") match {
      case _: FailedCursor =>
        c.as[Tweet]
      case _ => Right(DeleteTweet)
    }
  )
}

sealed trait TwitterObject
case class Tweet(id_str: String, text: String, entities: Entities) extends TwitterObject
case object DeleteTweet extends TwitterObject
case object ParseError extends TwitterObject

case class Entities(hashtags: List[Hashtag], urls: List[Url], media: Option[List[Media]])
case class Hashtag(indices: List[Int], text: String)

case class Url(expanded_url: String, unwound: Option[Unwound])
case class Unwound(url: String)

case class Media(`type`: String)




