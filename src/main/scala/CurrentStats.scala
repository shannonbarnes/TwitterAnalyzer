
final case class CurrentStats(
    allCount: Int,
    tweetCount: Int,
    deleteCount: Int,
    parseErrors: Int,
    ratePerSecond: Double,
    ratePerMinute: Double,
    ratePerHour: Double,
    percentWithEmojis: Double,
    percentWithUrls: Double,
    percentWithPhotos: Double,
    topHashTags: Seq[NameCount],
    topEmojis: Seq[NameCount],
    topDomains: Seq[NameCount]
)


final case class NameCount(name: String, count: Int)