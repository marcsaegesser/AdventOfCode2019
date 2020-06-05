package advent

import scala.annotation.tailrec

object Day25 {
  import IntMachine._

  def main(args: Array[String]): Unit = {
    playGame("", loadMachine(inputFile, List()))
  }

  @tailrec
  def playGame(input: String, machine: Machine): Unit = {
    val (o, m) = takeOutput(runMachine(provideInput(machine, input.toList.map(_.toLong) ++ List(10L))))
    val s = io.StdIn.readLine(o.map(_.toChar).mkString)
    playGame(s.trim, m)
  }

  val inputFile = "data/Day25.txt"
}
