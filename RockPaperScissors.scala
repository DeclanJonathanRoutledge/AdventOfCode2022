
import RockPaperScissors._

import scala.annotation.tailrec
import scala.io.Source
// A||X ROCK 1  pt2 x =lose
// B||Y PAPER 2 pt2 y = draw
// C||Z SCISSORS 3 pt z = win
// win 6
// draw 3
// loss 0
case class guide(enemyChoice:String, yourChoice :String)
case class roundScore(round:Int, letterScore: Int, totalForRound: Int)

val scissorsScore = 3
val paperScore = 2
val rockScore = 1

val loseScore = 0
val drawScore = 3
val winScore = 6

object RockPaperScissors{
  def scoreProccesor(data:Iterator[String]):Seq[guide] ={
   var choice:Seq[guide] = Seq()
     data.foreach{
      line => choice = choice:+ guide(line.head.toString, line.takeRight(1))
    }
    choice
  }
  def umpire(rounds:Seq[guide], translator : guide => roundScore): Seq[roundScore] =
    rounds.map(round => translator(round))


  def translatorPt1(round:guide):roundScore = round.enemyChoice match {
    case "A" => enemyARock(round)
    case "B" => enemyBPaper(round)
    case "C" => enemyCScissors(round)
  }
  def enemyARock(round:guide): roundScore=round.yourChoice match {
    case "X" => roundScore(drawScore,rockScore,drawScore + rockScore)//rock
    case "Y" => roundScore(winScore,paperScore,winScore + paperScore)//paper
    case "Z" => roundScore(loseScore,scissorsScore,loseScore + scissorsScore)//SCISSORS
  }
  def enemyBPaper(round:guide): roundScore=round.yourChoice match {
    case "X" => roundScore(loseScore,rockScore,loseScore + rockScore)//rock
    case "Y" => roundScore(drawScore,paperScore,drawScore + paperScore)//paper
    case "Z" => roundScore(winScore,scissorsScore,winScore + scissorsScore)//SCISSORS
  }
  def enemyCScissors(round:guide): roundScore=round.yourChoice match {
    case "X" => roundScore(winScore,rockScore,winScore + rockScore)//rock
    case "Y" => roundScore(loseScore,paperScore,loseScore + paperScore)//paper
    case "Z" => roundScore(drawScore,scissorsScore,drawScore + scissorsScore)//SCISSORS
  }

  def translatorPt2(round:guide):roundScore = round.yourChoice match {
    case "X" => losingGame(round)
    case "Y" => drawGame(round)
    case "Z" => winningGame(round)
  }
  def losingGame(round:guide): roundScore=round.enemyChoice match {
    case "A" => roundScore(loseScore,scissorsScore,loseScore + scissorsScore)//rock -- SCISSORS
    case "B" => roundScore(loseScore,rockScore,loseScore +rockScore)//paper -- rock
    case "C" => roundScore(loseScore,paperScore,loseScore +paperScore)//SCISSORS -- paper
  }
  def drawGame(round:guide): roundScore=round.enemyChoice match {
    case "A" => roundScore(drawScore,rockScore,drawScore + rockScore)//rock -- rock
    case "B" => roundScore(drawScore,paperScore,drawScore +paperScore)//paper -- paper
    case "C" => roundScore(drawScore,scissorsScore,drawScore +scissorsScore)//SCISSORS -- SCISSORS
  }
  def winningGame(round:guide): roundScore=round.enemyChoice match {
    case "A" => roundScore(winScore,paperScore,winScore + paperScore)//rock -- paper
    case "B" => roundScore(winScore,scissorsScore,winScore +scissorsScore)//paper -- SCISSORS
    case "C" => roundScore(winScore,rockScore,winScore +rockScore)//SCISSORS -- rock
  }

  @tailrec
  def totalScoreAdder(roundScores: Seq[roundScore], fullWeight:Int=0): Int = {
    if (roundScores.nonEmpty) totalScoreAdder(roundScores.drop(1), roundScores.head.totalForRound + fullWeight)
    else fullWeight
  }

}
val rawData = Source.fromFile("/Users/routld02/Documents/sideProjects/adventOfCode/plays.txt")
val rounds = scoreProccesor(rawData.getLines())

//---answer to part 1
println("total score determined by given moves: "+ totalScoreAdder(umpire(rounds, translatorPt1)))
println("match expected output: "+ (totalScoreAdder(umpire(rounds, translatorPt1)) equals 14297))
////---answer to part 2
println("total score determined by given outcome: "+ totalScoreAdder(umpire(rounds, translatorPt2)))
println("match expected output: "+  (totalScoreAdder(umpire(rounds, translatorPt2)) equals 10498))

rawData.close()