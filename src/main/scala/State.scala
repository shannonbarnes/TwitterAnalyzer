
trait MergeHelper[A, B] {
  def merge(a: A, b: B): B
}


object State {

  type CountMap = Map[String, Int]
  type ItemList = List[String]

  type ProcessState = State[ItemList]
  type CulmState = State[CountMap]

  implicit val listHelper: MergeHelper[ItemList, ItemList] = (a, b) => a ::: b

  implicit val mapHelper: MergeHelper[ItemList, CountMap] = new MergeHelper[ItemList, CountMap] {
    def merge(a: ItemList, b: CountMap): CountMap = a.foldLeft(b){case (m, v) => insertInv(m, v)}
    private def insertInv(m: CountMap, v : String): CountMap = m + ((v, m(v) + 1))
  }

  val emptyMap: CountMap = Map.empty.withDefaultValue(0)
  val emptyProcess: ConcreteState[ItemList] = ConcreteState(hashtags = List.empty, domains = List.empty, emojis = List.empty)
  val emptyCulmState: ConcreteState[CountMap] = ConcreteState(hashtags = emptyMap, domains = emptyMap, emojis = emptyMap)



}

case class ConcreteState[A](
  deleteCount: Int = 0,
  parseErrors: Int = 0,
  tweetCount: Int = 0,
  containedEmojiCount: Int = 0,
  containedUrlCount: Int = 0,
  containsPhotoCount: Int = 0,
  hashtags: A,
  domains: A,
  emojis: A,
) extends State[A] {

  def combine[B](a: State[B])(implicit ch: MergeHelper[B, A]): ConcreteState[A] = {
    copy(
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
  def deleteCount: Int
  def parseErrors: Int
  def tweetCount: Int
  def containedEmojiCount: Int
  def containedUrlCount: Int
  def containsPhotoCount: Int
  def hashtags: A
  def domains: A
  def emojis: A
}
