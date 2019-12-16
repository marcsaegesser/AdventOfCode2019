package advent

import scala.math.Integral.Implicits._

object Day16 {

  def run(): Unit = {
    val signal = readFile(inputFile)
    println(s"Day16.part1 = ${part1(signal)}")
    println(s"Day16.part2 = ${part2(signal)}")
  }

  def part1(signal: List[Int]): String =
    fft(signal, 100).mkString.take(8)

  def part2(signal: List[Int]): String = {
    val ss = List.fill(10000)(signal).flatten
    fftPart2(ss, 100).take(8).mkString
  }

  def fft(signal: List[Int], numPhases: Int): List[Int] = {
    def runPhase(input: List[Int]): List[Int] = {
      (1 to input.size).foldLeft(List.empty[Int]) { case (accum, digit) =>
        val (_, d) = Math.abs(input.zip(mkPattern(basePattern, digit)).map { case (a, b) => a*b }.sum) /% 10
        d +: accum
      }
    }.reverse

    (1 to numPhases).foldLeft(signal) { case (input, _) => runPhase(input) }
  }

  def fftPart2(signal: List[Int], numPhases: Int): List[Int] = {
    def runPhase(input: List[Int]): List[Int] = {
      input.reverse.foldLeft((List.empty[Int], 0)) { case ((accum, sum), d) =>
        val nextSum = sum + d
        ((nextSum % 10) +: accum, nextSum)
      }._1
    }

    val offset = signal.take(7).mkString.toInt
    (1 to numPhases).foldLeft(signal.drop(offset)) { case (input, _) => runPhase(input) }
  }

  def mkPattern(base: List[Int], n: Int): LazyList[Int] =
    LazyList.continually(base.flatMap(d => List.fill(n)(d))).flatten.drop(1)


  def parseInput(s: String): List[Int] =
    s.toList.map(_.asDigit)

  def readFile(f: String): List[Int] =
    parseInput(io.Source.fromFile(f).getLines().mkString)

  val basePattern = List(0, 1, 0, -1)

  val inputFile = "data/Day16.txt"

  val testData1 = "80871224585914546619083218645595"
  val testData2 = "19617804207202209144916044189917"
  val testData3 = "69317163492948606335995924319873"
}
