import org.scalatest._


class PriorityListTest extends FlatSpec with Matchers {
   def setup(p: PriorityList): Unit = {
     p.insert(List("A", "B", "A", "C"))
     p.insert("C")
     p.insert("C")
     p.insert("C")
     p.insert(List("D", "D", "D"))
     p.insert("D")
     p.insert("D")
     p.insert("A")
   }


  "PriorityList" should "return a list of ordered items" in {
    val p = new PriorityList(10)
    setup(p)

    println(p.sortedList)

    p.sortedList should be (Seq(NameCount("D", 5), NameCount("C", 4), NameCount("A", 3), NameCount("B", 1)))

  }

  it should "only return max items" in {
    val p = new PriorityList(2)
    setup(p)

    p.sortedList should be (Seq(NameCount("D", 5), NameCount("C", 4)))
  }

  it should "handle empty" in {
    val p = new PriorityList(40)
    p.sortedList should be (Seq.empty)
  }

}