package advent

object Day19 {
  import IntMachine._

  def run(): Unit = {
    val memory = readMemory(inputFile)
    println(s"Day19.part1 = ${part1(memory)}")
    println(s"Day19.part2 = ${part2(memory)}")
  }

  def part1(memory: Memory): Int =
    buildMap(memory).size

  def part2(memory: Memory): Long =
    findSquare(Coord(0, 0), memory).map(c => c.x*10000 + c.y).get

  def buildMap(memory: Memory): Set[Coord] = {
    (for {
      y <- 0L to 49
      x <- 0L to 49
      pos = Coord(x, y)
      v = checkDrone(pos, memory) if v
    } yield pos).toSet
  }

  def testSquare(pos: Coord, memory: Memory): Boolean = {
    checkDrone(pos + Coord(99, 0), memory) && checkDrone(pos + Coord(0, 99), memory)
  }

  def findStartOfRow(pos: Coord, memory: Memory): Option[Coord] = {
    def helper(pos: Coord, n: Int): Option[Coord] =
      if(n >= 10) None
      else if(checkDrone(pos, memory)) Some(pos)
      else helper(pos.right, n+1)

    helper(pos, 0)
  }

  def checkRow(pos: Coord, memory: Memory): (Coord, Option[Coord]) = {
    def helper(c: Coord): Option[Coord] =
      if(!checkDrone(c, memory))  // End of row
        None
      else if(!checkDrone(c + Coord(99, 0), memory))  // The rest of the row can't match
        None
      else if(checkDrone(c + Coord(0, 99), memory))   // Found it!
        Some(c)
      else
        helper(c.right)  // Check location to right

    val start = findStartOfRow(pos, memory)
    start match {
      case None    => (pos, None)
      case Some(c) => (c, helper(c))
    }
  }

  def findSquare(start: Coord, memory: Memory): Option[Coord] = {
    def helper(c: Coord, n: Int): Option[Coord] =
      if(n >= 1000) None
      else
        checkRow(c, memory) match {
          case (s, None)    => helper(s.down, n+1)
          case (s, f)       => f
        }

    helper(start, 0)
  }

  def showMap(map: Set[Coord]): String = {
    def showRow(y: Long): String =
      (0L to 49).foldLeft(new StringBuilder()) { case (sb, x) =>
        sb.append(if(map.contains(Coord(x, y))) '#' else '.')
      }.mkString

    (for {
      y <- (0L to 49)
      r =  showRow(y)
    } yield r).mkString("\n")
  }

  def checkDrone(pos: Coord, memory: Memory): Boolean =
    takeOutput(runMachine(initializeMachine(memory, List(pos.x, pos.y))))._1.head == 1L


  case class Coord(x: Long, y: Long) {
    def right: Coord = Coord(x+1, y)
    def down: Coord  = Coord(x,   y+1)

    def +(other: Coord): Coord =
      Coord(x + other.x, y + other.y)
  }

  val inputFile = "data/Day19.txt"
}
