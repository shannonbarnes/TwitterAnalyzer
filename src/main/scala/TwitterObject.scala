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
final case class Tweet(id_str: String, text: String, entities: Entities) extends TwitterObject
case object DeleteTweet extends TwitterObject
case object ParseError extends TwitterObject


final case class Entities(hashtags: List[Hashtag], urls: List[Url], media: Option[List[Media]])
final case class Hashtag(indices: List[Int], text: String)
final case class Url(expanded_url: String, unwound: Option[Unwound])
final case class Unwound(url: String)
final case class Media(`type`: String)




