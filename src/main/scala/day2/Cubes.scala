package day2

import scala.io.Source

object Cubes {
  case class Game(id: Int, red: Int, green: Int, blue: Int)

  private val colorMatch = "(\\d+)\\s+(red|green|blue)".r

  def parseGame(raw: String): Game = {
    def parseRounds(colorsInfo: String): (Int, Int, Int) = {
      colorsInfo.split(", ").foldLeft((0, 0, 0)) { (counts, singleColor) =>
        singleColor match {
          case colorMatch(count, color) => color match {
            case "red" => counts.copy(_1 = count.toInt + counts._1)
            case "green" => counts.copy(_2 = count.toInt + counts._2)
            case "blue" => counts.copy(_3 = count.toInt + counts._3)
          }
        }
      }
    }

    val s"Game $id: $cubes" = raw
    val rounds = cubes.split("; ")
    val (red, green, blue) = rounds.map(parseRounds).reduce((r1, r2) => (r1._1 + r2._1, r1._2 + r2._2, r1._3 + r2._3))

    Game(id.toInt, red, green, blue)
  }

  def main(args: Array[String]): Unit = {
    val validGames = Source.fromResource("day2_input.txt")
      .getLines()
      .map(parseGame)
      .filter { game =>
        // only 12 red cubes, 13 green cubes, and 14 blue cubes
        game.red <= 12 && game.green <= 13 && game.blue <= 14
      }

    println(validGames.map(_.id).sum)
  }
}
