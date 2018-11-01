import scala.collection.JavaConverters._
import scala.collection._
import java.util.concurrent.ConcurrentHashMap


case class NameCount(name: String, count: Int)

class PriorityList(maxToDisplay: Int) {
  val map: mutable.Map[String, Int] = new ConcurrentHashMap[String, Int]().asScala.withDefaultValue(0)
  def insert(vs: Seq[String]): Unit = vs foreach insert
  def insert(v: String): Unit = map += ((v, map(v) + 1))
  def sortedList: Seq[NameCount] = map.toSeq.sortWith(_._2 > _._2).take(maxToDisplay).map{case (name,count) => NameCount(name, count)}
}
