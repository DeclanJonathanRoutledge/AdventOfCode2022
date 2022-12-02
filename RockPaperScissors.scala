
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

val scissors = 3
val paper = 2
val rock = 1

val lose = 0
val draw = 3
val win = 6

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
    case "X" => roundScore(3,1,4)//rock
    case "Y" => roundScore(6,2,8)//paper
    case "Z" => roundScore(0,3,3)//SCISSORS
  }
  def enemyBPaper(round:guide): roundScore=round.yourChoice match {
    case "X" => roundScore(0,1,1)//rock
    case "Y" => roundScore(3,2,5)//paper
    case "Z" => roundScore(6,3,9)//SCISSORS
  }
  def enemyCScissors(round:guide): roundScore=round.yourChoice match {
    case "X" => roundScore(6,1,7)//rock
    case "Y" => roundScore(0,2,2)//paper
    case "Z" => roundScore(3,3,6)//SCISSORS
  }

  def translatorPt2(round:guide):roundScore = round.yourChoice match {
    case "X" => losingGame(round)
    case "Y" => drawGame(round)
    case "Z" => winningGame(round)
    // A||X ROCK 1  pt2 x =lose
    // B||Y PAPER 2 pt2 y = draw
    // C||Z SCISSORS 3 pt z = win
  }
  def losingGame(round:guide): roundScore=round.enemyChoice match {
    case "A" => roundScore(lose,scissors,lose + scissors)//rock -- SCISSORS
    case "B" => roundScore(lose,rock,lose +rock)//paper -- rock
    case "C" => roundScore(lose,paper,lose +paper)//SCISSORS -- paper
  }
  def drawGame(round:guide): roundScore=round.enemyChoice match {
    case "A" => roundScore(draw,rock,draw + rock)//rock -- rock
    case "B" => roundScore(draw,paper,draw +paper)//paper -- paper
    case "C" => roundScore(draw,scissors,draw +scissors)//SCISSORS -- SCISSORS
  }
  def winningGame(round:guide): roundScore=round.enemyChoice match {
    case "A" => roundScore(win,paper,win + paper)//rock -- paper
    case "B" => roundScore(win,scissors,win +scissors)//paper -- SCISSORS
    case "C" => roundScore(win,rock,win +rock)//SCISSORS -- rock
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
////---answer to part 2
println("total score determined by given outcome: "+ totalScoreAdder(umpire(rounds, translatorPt2)))


rawData.close()