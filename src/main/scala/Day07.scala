package advent

object Day07 {
  import IntMachine._

  def run(): Unit = {
    val memory = readMemory(inputFile)
    println(s"Day07.part1 = ${part1(memory)}")
    println(s"Day07.part2 = ${part2(memory)}")
  }

  def part1(memory: Memory): Long = {
    (0L to 4L).permutations.foldLeft(Long.MinValue) { case (o, ps) =>
      Math.max(o, runAmplifiers(ps, memory))
    }
  }

  def part2(memory: Memory): Long = {
    (5L to 9L).permutations.foldLeft(Long.MinValue) { case (o, ps) =>
      Math.max(o, runMachines(ps, memory).head)
    }
  }

  //========================
  // For part 1
  def runAmplifiers(phases: Seq[Long], memory: Memory): Long = {
    phases.foldLeft(0L) { case (i, p) =>
      runAmplifier(i, p, memory)
    }
  }

  def runAmplifier(input: Long, phase: Long, memory: Memory): Long = {
    runMachine(initializeMachine(memory, List(phase, input))).output.head
  }

  //========================
  // For part 2
  def startMachines(phases: Seq[Long], memory: Memory): (List[Long], Vector[Machine]) =
    phases.foldLeft((List(0L), Vector.empty[Machine])) { case ((i, ms), p) =>
      val (o, next) = takeOutput(runMachine(initializeMachine(memory, p +: i)))
      (o, ms :+ next)
    }

  def runMachines(phases: Seq[Long], memory: Memory): List[Long] = {
    def helper(f: List[Long], ms: Vector[Machine]): List[Long] =
      if(ms.last.state == Halted) f
      else {
        val (o, next) = runIteration(f, ms)
        helper(o, next)
      }


    val (f, ms) = startMachines(phases, memory)
    helper(f, ms)
  }

  def runIteration(feedback: List[Long], machines: Vector[Machine]): (List[Long], Vector[Machine]) =
    machines.foldLeft((feedback, Vector.empty[Machine])) { case ((i, ms), m) =>
      val (o, next) = takeOutput(runAmplifier(i, m))
      (o, ms :+ next)
    }

  def runAmplifier(input: List[Long], machine: Machine): Machine =
    runMachine(provideInput(machine, input))

  val inputFile = "data/Day07.txt"

  val test1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
  val test2 = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
  val test3 = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"

  val test4 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
  val test5 = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
}
