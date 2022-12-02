
import elfInventory._

import scala.annotation.tailrec
import scala.io.Source

case class Elf(elfName: String, elfLoad: Int)

object elfInventory{

  def elfProccesor(data:Iterator[String]):Seq[Elf] = {
  var elfsPosition = 1
  val elf0: Elf = Elf("elf number: 0", 0)
  var allElfs: Seq[Elf] = Seq(elf0)
  var sum :Int = 0
    data.foreach(line => {
      if (line.isEmpty) {
        allElfs =allElfs:+Elf(s"elf number: $elfsPosition", sum)
        elfsPosition = elfsPosition+1
        sum =  0
      }
      else {sum = sum +line.toInt}
    })
  allElfs
}
  def elfHeavyLifter(elfSeq: Seq[Elf]): Elf ={
   val seqOfCalories = for{
     elf<-elfSeq
   }yield{
     elf.elfLoad
   }
   val elfsPosition= seqOfCalories.zipWithIndex.maxBy(_._1)._2
    Elf(s"elf number: $elfsPosition", seqOfCalories.max)
  }
  def elfSortedSeq(elfSeq: Seq[Elf]): Seq[Elf] = elfSeq.sortBy(_.elfLoad)
  @tailrec
  def elfCalorieAdder(elfSeq: Seq[Elf], fullWeight:Int): Int = {
      if (elfSeq.nonEmpty) elfCalorieAdder(elfSeq.drop(1), elfSeq.head.elfLoad + fullWeight)
      else fullWeight
  }

}
val rawData = Source.fromFile("/Users/routld02/Documents/sideProjects/adventOfCode/elfInventry.txt")
val elfSeq = elfProccesor(rawData.getLines())
//---answer to part 1
println("max weight carried by an individual elf: "+elfCalorieAdder(elfSortedSeq(elfSeq).takeRight(1),0))
println("max carrying elf: " +elfHeavyLifter(elfSeq))
//---answer to part 2
println("max weight carried by three elfs: "+elfCalorieAdder(elfSortedSeq(elfSeq).takeRight(3),0))
println("top carrying three elfs: " + elfSortedSeq(elfSeq).takeRight(3))

rawData.close()