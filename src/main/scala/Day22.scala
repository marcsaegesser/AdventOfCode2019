package advent

object Day22 {

  def run(): Unit = {
    println(s"Day22.part1 = ${part1()}")
    println(s"Day22.part2 = ${part2()}")
  }

  def part1() = {
    val ps = parseInput(readFile(inputFile), 10007)
    val p = ps.reduce(compose)
    runPermutation(p, 2019)
  }

  def part2() = {
    val ps = parseInput(readFile(inputFile), 119315717514047L)
    val p = ps.reduce(compose)
    val bigP = stimes(p, compose, 101741582076661L)
    runPermutation(invert(bigP), 2020)
  }

  case class Permutation(a: BigInt, b: BigInt, sz: BigInt)

  def normalize(x: BigInt, m: BigInt): BigInt =
    (x % m) match {
      case n if n < 0 => n + m
      case n          => n
    }

  def stimes[T](p: T, f: (T, T) => T, n: BigInt): T = {
    def helper(pp: T, count: BigInt): T = {
      if(count == n)                pp
      else if((n-count) >= 2*count) helper(f(pp, pp), count*2)
      else if((n-count) >= 2)       f(pp, stimes(p, f, n-count))
      else                          helper(f(pp, p), count+1)
    }

    helper(p, 1)
  }

  // Compute where the permutation takes index x
  def runPermutation(perm: Permutation, x: BigInt): BigInt =
    perm match { case Permutation(a, b, sz) =>
      normalize(a * x + b, sz)
    }

  def compose(p1: Permutation, p2: Permutation): Permutation =
    (p1, p2) match { case (Permutation(a, b, sz), Permutation(ap, bp, _)) =>
      Permutation(normalize(a*ap, sz), normalize(ap * b + bp, sz), sz)
    }

  def invert(p: Permutation): Permutation =
    p match { case Permutation(a, b, sz) =>
      val mul = (x: BigInt, y: BigInt) => normalize(x * y, sz)
      val ap = normalize(stimes[BigInt](a, mul, sz-2), sz)
      val bp = -normalize(ap * b, sz)
      Permutation(ap, bp, sz)
    }

  val parseDealNewStack = """deal into new stack""".r
  val parseCutN         = """cut (-?\d+)""".r
  val parseDealWithIncr = """deal with increment (\d+)""".r

  def parseLine(line: String, sz: BigInt): Permutation =
    line match {
      case parseCutN(n)         => Permutation(1,        -n.toLong, sz)
      case parseDealWithIncr(n) => Permutation(n.toLong,  0,        sz)
      case parseDealNewStack    => Permutation(-1,        sz-1,     sz)
    }

  def parseInput(lines: List[String], sz: Long): List[Permutation] =
    lines.map(parseLine(_, sz))

  def readFile(f: String): List[String] =
    io.Source.fromFile(f)
      .getLines()
      .toList

  val inputFile = "data/Day22.txt"

  val testData1 =
    """deal with increment 7
      |deal into new stack
      |deal into new stack""".stripMargin
  val expected1 = Vector(0, 3, 6, 9, 2, 5, 8, 1, 4, 7)

  val testData2 =
    """cut 6
      |deal with increment 7
      |deal into new stack""".stripMargin
  val expected2 = Vector(3, 0, 7, 4, 1, 8, 5, 2, 9, 6)

  val testData3 =
    """deal with increment 7
      |deal with increment 9
      |cut -2""".stripMargin
  val expected3 = Vector(6, 3, 0, 7, 4, 1, 8, 5, 2, 9)

  val testData4 =
    """deal into new stack
      |cut -2
      |deal with increment 7
      |cut 8
      |cut -4
      |deal with increment 7
      |cut 3
      |deal with increment 9
      |deal with increment 3
      |cut -1""".stripMargin
  val expected4 = Vector(9, 2, 5, 8, 1, 4, 7, 0, 3, 6)
}
