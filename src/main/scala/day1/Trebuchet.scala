package day1

import scala.io.Source

object Trebuchet {
  private val spelledDigits = "one|two|three|four|five|six|seven|eight|nine"
  private val spelledDigitsIndex = spelledDigits.split('|').zipWithIndex.toMap
  private val digitsMatcher = s"$spelledDigits|\\d".r

  def main(args: Array[String]): Unit = {
    def asDigit(matched: String): String = spelledDigitsIndex.get(matched).map(_ + 1).getOrElse(matched).toString

    def solvePart1(): Int =
      Source.fromResource("day1_input.txt")
        .getLines()
        .flatMap { line =>
          for {
            r <- line.find(_.isDigit)
            l <- line.findLast(_.isDigit)
          } yield s"$r$l".toInt
        }.sum

    def solvePart2(): Int = {
      Source.fromResource("day1_input.txt")
        .getLines()
        .flatMap { line =>
          val all: Vector[String] = Vector.unfold(line) { line =>
            for {
              m <- digitsMatcher.findFirstMatchIn(line)
              di = asDigit(m.matched)
            } yield (di, line.substring(m.start + 1))
          }
          for {
            r <- all.headOption
            l <- all.lastOption
          } yield s"$r$l".toInt
        }.sum
    }

    println(solvePart1())
    println(solvePart2())
  }
}
