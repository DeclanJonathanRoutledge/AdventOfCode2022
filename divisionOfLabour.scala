
import divisionOfLabour._

import scala.annotation.tailrec
import scala.io.Source

case class JobRanges(rangeOne:Set[Int], rangeTwo:Set[Int])
case class JobAreas(lowRangeOne:Int, HighRangeOne:Int, lowRangeTwo:Int, HighRangeTwo:Int)
case class Subset(isSubset: Boolean, isSubsetValue: Int)

val letterPriorities = ('a' to 'z').zip(1 to 26).toMap ++ ('A' to 'Z').zip(27 to 52).toMap

object divisionOfLabour{
  def jobSorter(data:Iterator[String]):Seq[JobAreas] = {
    var jobs:Seq[JobAreas] = Seq()
    data.foreach{
      line =>  val range= line.split("[,\\-]")

        jobs = jobs :+JobAreas( range.head.toInt,
          range.take(2).takeRight(1).head.toInt,
          range.take(3).takeRight(1).head.toInt,
          range.takeRight(1).head.toInt)
    }
    jobs
  }


  def rangeSorter(jobAreas:Seq[JobAreas]):Seq[JobRanges] = {
    var jobRanges:Seq[JobRanges] = Seq()
    jobAreas.foreach{
      line => jobRanges = jobRanges:+JobRanges((line.lowRangeOne to line.HighRangeOne).toSet, (line.lowRangeTwo to line.HighRangeTwo).toSet)
    }
    jobRanges
  }
//subsetOf
  def subsetChecker(ranges:Seq[JobRanges]):Seq[Subset] ={
    var listOfSubsets:Seq[Subset] = Seq()
    ranges.foreach{
      range =>
        val firstCheck = range.rangeOne subsetOf range.rangeTwo
        val secondCheck = range.rangeTwo subsetOf range.rangeOne
        if(firstCheck | secondCheck) {
        listOfSubsets = listOfSubsets :+ Subset(isSubset = true, 1)
                                      }
        else{
          listOfSubsets = listOfSubsets :+ Subset(isSubset = false, 0)
            }
    }
    listOfSubsets

  }

  def CrossOverChecker(ranges:Seq[JobRanges]):Seq[Subset] ={
    var listOfSubsets:Seq[Subset] = Seq()
    ranges.foreach{
      range =>
        val intersectCheck = range.rangeOne intersect range.rangeTwo
        if(intersectCheck.nonEmpty) {
          listOfSubsets = listOfSubsets :+ Subset(isSubset = true, 1)
        }
        else{
          listOfSubsets = listOfSubsets :+ Subset(isSubset = false, 0)
        }
    }
    listOfSubsets

  }


  def commonality(abc: Seq[String]):String = {
    (abc.head intersect abc.tail.head) intersect abc.takeRight(1).head//intersect abc.takeRight(1)
  }

  @tailrec
  def totalAdder(value: Seq[Subset], fullWeight:Int=0): Int = {
    if (value.nonEmpty) totalAdder(value.drop(1), value.head.isSubsetValue + fullWeight)
    else fullWeight
  }
  def processor(data:Iterator[String]):Int= totalAdder(subsetChecker(rangeSorter(jobSorter(data))))
  def processorPt2(data:Iterator[String]):Int= totalAdder(CrossOverChecker(rangeSorter(jobSorter(data))))
}
//test part 1
println("Test part 1")
println("____________")
val testData = Source.fromFile("/Users/routld02/Documents/sideProjects/adventOfCode/testResources/exampleShiftDivision.txt")
val test1Answer = processor(testData.getLines())
println("Part One -total test duplicate work: "+test1Answer)
print(test1Answer equals (2))
println(" response")
println("____________")
testData.close()
//---answer to part 1
println("Answer to part 1")
println("____________")
val rawData = Source.fromFile("/Users/routld02/Documents/sideProjects/adventOfCode/resources/shiftDivision.txt")
println("Part One -total duplicate work: "+processor(rawData.getLines()))
println("____________")
rawData.close()

println()

//test part 2
println("Test part 2")
println("____________")
val testDataTwo = Source.fromFile("/Users/routld02/Documents/sideProjects/adventOfCode/testResources/exampleShiftDivision.txt")
val test2Answer = processorPt2(testDataTwo.getLines())
println("Part two -total test duplicate work: "+test2Answer)
print(test2Answer equals (4))
println(" response")
println("____________")
testDataTwo.close()
//---answer to part 2
println("Answer to part 2")
println("____________")
val rawDataTwo = Source.fromFile("/Users/routld02/Documents/sideProjects/adventOfCode/resources/shiftDivision.txt")
println("Part Two -total duplicate work: "+processorPt2(rawDataTwo.getLines()))
println("____________")
rawDataTwo.close()

