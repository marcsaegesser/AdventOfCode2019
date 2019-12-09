package advent

object Day09 {
  import IntMachine._

  def run(): Unit = {
    val memory = readMemory(inputFile)
    println(s"Day09.part1 = ${part1(memory)}")
    println(s"Day09.part2 = ${part2(memory)}")
  }

  def part1(memory: Memory): Long =
    takeOutput(runMachine(initializeMachine(memory, List(1))))._1.reverse.head

  def part2(memory: Memory): Long =
    takeOutput(runMachine(initializeMachine(memory, List(2))))._1.reverse.head

  val inputFile = "data/Day09.txt"

  val test1 = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
  val test2 = "1102,34915192,34915192,7,4,7,99,0"
  val test3 = "104,1125899906842624,99"
}
