package advent

object Day05 {
  import IntMachine._

  def run(): Unit = {
    val memory = readMemory(inputFile)

    println(s"Day05.part1 = ${part1(memory)}")
    println(s"Day05.part2 = ${part2(memory)}")
  }

  def part1(memory: Memory): Long =
    runMachine(initializeMachine(memory, List(1))).output.head

  def part2(memory: Memory): Long =
    runMachine(initializeMachine(memory, List(5))).output.head


  val inputFile = "data/Day05.txt"

  val testData1 = "1002,4,3,4,33"
  val testData2 = "1101,100,-1,4,0"
}
