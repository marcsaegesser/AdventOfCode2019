package advent

object Day01 {

  def day01(): Unit = {
    val input = readFile(inputFile)

    println(s"Day01.part1 = ${part1(input)}")
    println(s"Day01.part2 = ${part2(input)}")
  }

  def part1(masses: Seq[Int]): Int =
    masses.map(computeFuel).sum

  def part2(masses: Seq[Int]): Int =
    masses.map(computeAllFuel).sum

  def computeFuel(mass: Int): Int =
    mass / 3 - 2

  def computeAllFuel(mass: Int): Int = {
    def helper(accum: Int, m: Int): Int = {
      val next = computeFuel(m)
      if(next <= 0) accum
      else          helper(accum + next, next)
    }

    helper(0, mass)
  }

  def readFile(f: String): Seq[Int] =
    io.Source.fromFile(f)
      .getLines()
      .map(_.toInt)
      .toList

  val inputFile = "data/Day01.txt"
}
