
import org.scalatest._
import TwitterObject._
import jawn.Parser

import scala.util.Success


class DecodeTwitterObjectTest extends FlatSpec with Matchers {


  implicit val f = io.circe.jawn.CirceSupportParser.facade
  
  "TwitterObject decoder" should "decode a delete object" in {
    val json =
      """{
        |   "delete":{
        |      "status":{
        |         "id":220753647653494800,
        |         "id_str":"220753647653494784",
        |         "user_id":243513309,
        |         "user_id_str":"243513309"
        |      },
        |      "timestamp_ms":"1519765900931"
        |   }
        |}""".stripMargin

    val Success(myResult) = Parser.parseFromString(json)
    
    myResult.as[TwitterObject] should be (Right(DeleteTweet))
  }


  it should "decode a tweet object" in {
    val json =
      """{
        |  "created_at": "Thu Apr 06 15:24:15 +0000 2017",
        |  "id_str": "850006245121695744",
        |  "text": "1\/ Today we\u2019re sharing our vision for the future of the Twitter API platform!\nhttps:\/\/t.co\/XweGngmxlP",
        |  "user": {
        |    "id": 2244994945,
        |    "name": "Twitter Dev",
        |    "screen_name": "TwitterDev",
        |    "location": "Internet",
        |    "url": "https:\/\/dev.twitter.com\/",
        |    "description": "Your official source for Twitter Platform news, updates & events. Need technical help? Visit https:\/\/twittercommunity.com\/ \u2328\ufe0f #TapIntoTwitter"
        |  },
        |  "place": {
        |  },
        |  "entities": {
        |    "hashtags": [
        |    ],
        |    "urls": [
        |      {
        |        "url": "https:\/\/t.co\/XweGngmxlP",
        |        "expanded_url": "https:\/\/cards.twitter.com\/cards\/18ce53wgo4h\/3xo1c",
        |        "unwound": {
        |          "url": "https:\/\/cards.twitter.com\/cards\/18ce53wgo4h\/3xo1c",
        |          "title": "Building the Future of the Twitter API Platform"
        |        }
        |      }
        |    ],
        |    "user_mentions": [
        |    ]
        |  }
        |}""".stripMargin

    val Success(myResult) = Parser.parseFromString(json)
    val Right(tweet: Tweet) = myResult.as[TwitterObject]
    tweet.id_str should be ("850006245121695744")
  }



}