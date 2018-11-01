# TwitterAnalyzer

Steams sample Twitter API and analyzes tweets and gathers stats.
Uses `http4s` client and server, and `fs2` with `cats-effect`

Perform a sbt run to start program

    sbt run

Refresh the following link to get up to date stats

    http://localhost:8080/stats


    {
      "allCount": 1745,
      "tweetCount": 1577,
      "deleteCount": 168,
      "parseErrors": 0,
      "ratePerSecond": 37.93,
      "ratePerMinute": 2235,
      "ratePerHour": 126135,
      "percentWithEmojis": 16.55,
      "percentWithUrls": 21.31,
      "percentWithPhotos": 20.1,
      "topHashTags": [
        {
          "name": "Rayito",
          "count": 6
        },
        {
          "name": "SibroGantengSemriwing",
          "count": 5
        },
        {
          "name": "BTS",
          "count": 4
        },
        {
          "name": "Rayito3",
          "count": 3
        },
        {
          "name": "Rayito2",
          "count": 3
        },
        {
          "name": "1FIRST",
          "count": 3
        },
        {
          "name": "MTVEMABiggestFansBTS",
          "count": 3
        },
        {
          "name": "اقترح_وظيفه_للسيسي",
          "count": 2
        },
        {
          "name": "الاتحاد",
          "count": 2
        },
        {
          "name": "FunkoNYCC",
          "count": 2
        }
      ],
      "topEmojis": [
        {
          "name": "😂",
          "count": 93
        },
        {
          "name": "🔥",
          "count": 29
        },
        {
          "name": "❤",
          "count": 28
        },
        {
          "name": "😍",
          "count": 21
        },
        {
          "name": "😭",
          "count": 19
        },
        {
          "name": "👏",
          "count": 15
        },
        {
          "name": "🙄",
          "count": 13
        },
        {
          "name": "⚡",
          "count": 12
        },
        {
          "name": "♥",
          "count": 9
        },
        {
          "name": "🙏",
          "count": 9
        }
      ],
      "topDomains": [
        {
          "name": "twitter.com",
          "count": 181
        },
        {
          "name": "du3a.org",
          "count": 24
        },
        {
          "name": "youtu.be",
          "count": 11
        },
        {
          "name": "www.facebook.com",
          "count": 11
        },
        {
          "name": "bit.ly",
          "count": 6
        },
        {
          "name": "curiouscat.me",
          "count": 6
        },
        {
          "name": "dlvr.it",
          "count": 6
        },
        {
          "name": "ift.tt",
          "count": 4
        },
        {
          "name": "www.instagram.com",
          "count": 4
        },
        {
          "name": "pbs.twimg.com",
          "count": 3
        }
      ]
    }