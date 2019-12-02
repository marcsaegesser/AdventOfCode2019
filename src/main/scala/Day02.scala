package advent

object Day02 {

  def day02(): Unit = {
    val memory = readFile(inputFile)
    println(s"Day02.part1 = ${part1(memory)}")
    println(s"Day02.part2 = ${part2(memory)}")
  }

  def part1(memory: Vector[Int]): Int =
    run(initializeMachine(memory, 12, 2)).result

  def part2(memory: Vector[Int]): Int = {
    val is = LazyList.from(0).take(100) // The integers 0 to 99 as a LazyList
    val (n, v) =
      is.flatMap(i => is.map((i, _))) // The tuples (0, 0), (0, 1) ... (99,99) as a LazyList
        .find { case (n, v) => run(initializeMachine(memory, n, v)).result == 19690720 }.get  // Assume there is a solution
    n*100 + v
  }

  sealed trait State
  case object Running extends State
  case object Halted extends State

  val AddInstruction  = 1
  val MultInstruction = 2
  val HaltInstruction = 99

  def run(m: Machine): Machine = {
    m.state match {
      case Halted  => m
      case Running => run(step(m))
    }
  }

  def step(m: Machine): Machine = {
    m match {
      case (Machine(ip, state, m)) if state == Running  =>
        m(ip) match {
          case AddInstruction  => Machine(ip+4,  Running, m.updated(m(ip+3), m(m(ip+1))+m(m(ip+2))))
          case MultInstruction => Machine(ip+4,  Running, m.updated(m(ip+3), m(m(ip+1))*m(m(ip+2))))
          case HaltInstruction => Machine(ip,    Halted,  m)
        }
    }
  }

  case class Machine(ip: Int, state: State, memory: Vector[Int]) {
    def result: Int = memory(0)
  }

  def initializeMachine(memory: Vector[Int], n: Int, v: Int): Machine =
    Machine(0, Running, memory.updated(1, n).updated(2, v))

  def readFile(f: String): Vector[Int] =
    io.Source.fromFile(f)
      .getLines().flatten
      .mkString
      .split(",")
      .toVector
      .map(_.toInt)

  val inputFile = "data/Day02.txt"

  val test1 = Vector(1,0,0,0,99)
  val test2 = Vector(2,3,0,3,99)
  val test3 = Vector(2,4,4,5,99,0)
  val test4 = Vector(1,1,1,4,99,5,6,0,99)
}
