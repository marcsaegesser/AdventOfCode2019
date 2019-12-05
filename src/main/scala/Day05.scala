package advent

object Day05 {
  import IntMachine._

  def run(): Unit = {
    val memory = readFile(inputFile)

    println(s"Day05.part1 = ${part1(memory)}")
    println(s"Day05.part2 = ${part2(memory)}")
  }

  def part1(memory: Vector[Int]): Int =
    runMachine(initializeMachine(memory, List(1))).output.head

  def part2(memory: Vector[Int]): Int =
    runMachine(initializeMachine(memory, List(5))).output.head

  def readFile(f: String): Vector[Int] =
    io.Source.fromFile(f)
      .getLines().flatten
      .mkString
      .split(",")
      .toVector
      .map(_.toInt)

  def parseString(s: String): Vector[Int] =
    s.split(",")
      .toVector
      .map(_.toInt)


  val inputFile = "data/Day05.txt"

  val testData1 = "1002,4,3,4,33"
  val testData2 = "1101,100,-1,4,0"
}
