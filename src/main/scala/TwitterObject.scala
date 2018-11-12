import io.circe.{Decoder, FailedCursor}
import io.circe.generic.auto._
import State._

object TwitterObject {

  implicit val decodeTwitterObject: Decoder[TwitterObject] = Decoder.instance(c =>
    c.downField("delete") match {
      case _: FailedCursor =>
        c.as[Tweet]
      case _ => Right(DeleteTweet)
    }
  )
}

sealed trait TwitterObject extends ProcessState

final case class Tweet(id_str: String, text: String, entities: Entities) extends TweetExtration with TwitterObject
case object DeleteTweet extends ConcreteState(deleteCount = 1, domains = List.empty, emojis = List.empty, hashtags = List.empty) with TwitterObject
case object ParseError extends ConcreteState(parseErrors = 1,  domains = List.empty, emojis = List.empty, hashtags = List.empty) with TwitterObject


final case class Entities(hashtags: List[Hashtag], urls: List[Url], media: Option[List[Media]])
final case class Hashtag(indices: List[Int], text: String)
final case class Url(expanded_url: String, unwound: Option[Unwound])
final case class Unwound(url: String)
final case class Media(`type`: String)