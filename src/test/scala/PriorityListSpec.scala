import State._
import org.scalatest._

class PriorityListSpec extends FlatSpec with Matchers {


  def setup: CountMap = {
    val map: CountMap = emptyMap
    mapHelper.merge(List("A", "B", "A", "C", "C", "C", "C", "D", "D", "D", "D", "D", "A"), map)
   }

  "PriorityList" should "return a list of ordered items" in {
    setup.sortedList() should be (Seq(NameCount("D", 5), NameCount("C", 4), NameCount("A", 3), NameCount("B", 1)))

  }

  it should "only return max items" in {
    setup.sortedList(2) should be (Seq(NameCount("D", 5), NameCount("C", 4)))
  }

  it should "handle empty" in {
    Map.empty[String, Int].sortedList() should be (Seq.empty)
  }
}