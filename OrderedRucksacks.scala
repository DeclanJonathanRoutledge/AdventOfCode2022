
import OrderedRucksacks._

import scala.annotation.tailrec
import scala.io.Source

case class rucksack(compartmentOne:String, compartmentTwo :String, CommonLetter: String)
case class Priority(commonLetter: String, priority: Int)

val letterPriorities = ('a' to 'z').zip(1 to 26).toMap ++ ('A' to 'Z').zip(27 to 52).toMap

object OrderedRucksacks{
  def bagSorter(data:Iterator[String]):Seq[rucksack] = {
    var bags:Seq[rucksack] = Seq()
    data.foreach{
      line =>
        bags = bags:+ rucksack(line.take(line.length/2),
          line.takeRight(line.length/2),
          line.take(line.length/2) intersect line.takeRight(line.length/2))
    }
    bags
  }
  def priorityChecker(bags:Seq[rucksack]): Seq[Priority] ={
    var priorities:Seq[Priority] = Seq()
    bags.foreach{
      bag =>
        priorities = priorities:+ Priority(bag.CommonLetter,letterPriorities(bag.CommonLetter.head))
    }
    priorities
  }

  def bigBagSorter(data:Iterator[String]):Seq[Priority] ={
    var bags:Seq[Priority] = Seq()
    data.grouped(3).foreach{
      line =>
        bags = bags:+ Priority(commonality(line), letterPriorities(commonality(line).head))
    }
    bags
  }
  def commonality(abc: Seq[String]):String = {
    (abc.head intersect abc.tail.head) intersect abc.takeRight(1).head//intersect abc.takeRight(1)
  }

  @tailrec
  def totalScoreAdder(bags: Seq[Priority], fullWeight:Int=0): Int = {
    if (bags.nonEmpty) totalScoreAdder(bags.drop(1), bags.head.priority + fullWeight)
    else fullWeight
  }

}

//---answer to part 1
val rawData = Source.fromFile("/Users/routld02/Documents/sideProjects/adventOfCode/rucksacks.txt")
println("Part One -total priorities:  "+totalScoreAdder(priorityChecker(bagSorter(rawData.getLines()))))
rawData.close()

////---answer to part 2
val rawDataTwo = Source.fromFile("/Users/routld02/Documents/sideProjects/adventOfCode/rucksacks.txt")
println("Part Two -total priorities:  " + totalScoreAdder(bigBagSorter(rawDataTwo.getLines())))
rawDataTwo.close()