import com.typesafe.config.ConfigFactory

trait MergeHelper[A, B] {
  def merge(a: A, b: B): B
}

object ConcreteState {

  type CountMap = Map[String, Int]
  type ItemList = List[String]

  type ProcessedTweets = State[ItemList]
  type CumulativeState = ConcreteState[CountMap]

  implicit val listHelper: MergeHelper[ItemList, ItemList] = (a, b) => a ::: b

  implicit val mapHelper: MergeHelper[ItemList, CountMap] = (a, b) => a.foldLeft(b){case (m, v) => m + ((v, m(v) + 1))}

  val displayCount: Int = ConfigFactory.load.getInt("maxTopDisplay")

  implicit class CountMapOps(map: CountMap) {
    def sortedList(num: Int = displayCount): Seq[NameCount] =
      map.toSeq.sortWith(_._2 > _._2).take(num).map{case (name,count) => NameCount(name, count)}
  }

  val emptyMap: CountMap = Map.empty.withDefaultValue(0)
  val emptyProcess: ConcreteState[ItemList] = new ConcreteState(List.empty)
  val emptyCumulativeState: CumulativeState = new ConcreteState(emptyMap)
}

case class ConcreteState[A](
  seconds: Int,
  deleteCount: Int,
  parseErrors: Int,
  tweetCount: Int,
  containedEmojiCount: Int,
  containedUrlCount: Int,
  containsPhotoCount: Int,
  hashtags: A,
  domains: A,
  emojis: A,
) extends State[A] {

  def this(empty: A) {
    this(0, 0, 0, 0, 0, 0, 0, empty, empty, empty)
  }

  def incSeconds: ConcreteState[A] = {
    this.copy(seconds = seconds + 1)
  }

  def combine[B](a: State[B])(implicit ch: MergeHelper[B, A]): ConcreteState[A] = {
    copy (
      deleteCount = deleteCount + a.deleteCount,
      parseErrors = parseErrors + a.parseErrors,
      tweetCount = tweetCount + a.tweetCount,
      containedEmojiCount = containedEmojiCount + a.containedEmojiCount,
      containedUrlCount = containedUrlCount + a.containedUrlCount,
      containsPhotoCount = containsPhotoCount + a.containsPhotoCount,
      hashtags = ch.merge(a.hashtags, hashtags),
      domains = ch.merge(a.domains, domains),
      emojis = ch.merge(a.emojis, emojis)
    )
  }
}

trait State[A] {
  def seconds: Int
  def deleteCount: Int
  def parseErrors: Int
  def tweetCount: Int
  def containedEmojiCount: Int
  def containedUrlCount: Int
  def containsPhotoCount: Int
  def hashtags: A
  def domains: A
  def emojis: A
  def all: Int = deleteCount + parseErrors + tweetCount
}