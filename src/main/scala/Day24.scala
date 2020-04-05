package advent

object Day24 {

  def day24(): Unit = {
    val tiles = readFile(inputFile)
    println(s"day24.part1 = ${part1(tiles)}")
    println(s"day24.part2 = ${part2(tiles, 200)}")
  }

  def part1(tiles: Tiles): Int =
    biodiversityRating(runUntilDuplicate(tiles))

  def part2(tiles: Tiles, n: Int): Int =
    runSteps2(tiles, n).size

  def runUntilDuplicate(tiles: Tiles): Tiles = {
    def helper(accum: Set[Tiles], current: Tiles): Tiles = {
      val next = runStep(current)
      if(accum.contains(next)) next
      else                     helper(accum + next, next)
    }


    helper(Set(), tiles)
  }

 def runStep(tiles: Tiles) = {
    (for {
      y <- 0 to 4
      x <- 0 to 4
      n =  nextValue(tiles, Coord(x, y, 0))
    } yield n).flatten.toSet
  }

  def nextValue(tiles: Tiles, c: Coord): Option[Coord] = {
    val count = countAdjacent(tiles, c)
    if(tiles(c)) {
      if(count !=  1) None
      else           Some(c)
    } else {
      if(count == 1 || count == 2) Some(c)
      else                         None
    }
  }

  def countAdjacent(tiles: Tiles, c: Coord): Int =
    c.adjacencies1
      .map(tiles.contains)
      .filter(_ == true)
      .size

  def biodiversityRating(tiles: Tiles): Int = {
    tiles.toList.map(c => biodiversityValues(c)).sum
  }

  val biodiversityValues = {
    (for {
      y <- 0 to 4
      x <- 0 to 4
     } yield Coord(x, y, 0))
      .foldLeft((Map.empty[Coord, Int], 1)) { case ((m, v), c) => (m + (c -> v), v*2) }
      ._1
  }

  def countAdjacent2(tiles: Tiles, c: Coord): Int =
    c.adjacencies2
      .filter(tiles.contains)
      .size

  def runStep2(tiles: Tiles): Tiles = {
    def updateBug(c: Coord): Option[Coord] =
      if(countAdjacent2(tiles, c) == 1) Some(c)
      else                              None

    def updateSpace(c: Coord): Option[Coord] = {
      val count = countAdjacent2(tiles, c)
      if(count == 1 || count == 2) Some(c)
      else                         None
    }

    val updateBugs = tiles.map(updateBug).flatten
    val updatedSpaces = tiles.map(_.adjacencies2).flatten.filterNot(tiles.contains).map(updateSpace).flatten

    updateBugs ++ updatedSpaces
  }

  def runSteps2(tiles: Tiles, n: Int): Tiles =
    if(n == 0) tiles
    else       runSteps2(runStep2(tiles), n-1)

  val MIN_COORD = 0
  val MAX_COORD = 4
  val CENTER_COORD = 2

  case class Coord(x: Int, y: Int, level: Int) {
    def isCenter(x: Int, y: Int): Boolean = x == CENTER_COORD && y == CENTER_COORD

    def up =
      this match {
        case Coord(x, y, l) if y == MIN_COORD    => Set(Coord(CENTER_COORD, CENTER_COORD-1, l-1))
        case Coord(x, y, l) if isCenter(x, y-1)  => (MIN_COORD to MAX_COORD).map(x => Coord(x, MAX_COORD, l+1)).toSet
        case Coord(x, y, l)                      => Set(Coord(x, y-1, l))
      }

    def down =
      this match {
        case Coord(x, y, l) if y == MAX_COORD   => Set(Coord(CENTER_COORD, CENTER_COORD+1, l-1))
        case Coord(x, y, l) if isCenter(x, y+1) => (MIN_COORD to MAX_COORD).map(x => Coord(x, MIN_COORD, l+1)).toSet
        case Coord(x, y, l)                     => Set(Coord(x, y+1, l))
      }

    def right =
      this match {
        case Coord(x, y, l) if x == MAX_COORD   => Set(Coord(CENTER_COORD+1, CENTER_COORD, l-1))
        case Coord(x, y, l) if isCenter(x+1, y) => (MIN_COORD to MAX_COORD).map(y => Coord(MIN_COORD, y, l+1)).toSet
        case Coord(x, y, l)                     => Set(Coord(x+1, y, l))
      }

    def left =
      this match {
        case Coord(x, y, l) if x == MIN_COORD   => Set(Coord(CENTER_COORD-1, CENTER_COORD, l-1))
        case Coord(x, y, l) if isCenter(x-1, y) => (MIN_COORD to MAX_COORD).map(y => Coord(MAX_COORD, y, l+1)).toSet
        case Coord(x, y, l)                     => Set(Coord(x-1, y, l))
      }

    def adjacencies1: Seq[Coord] = Seq(Coord(x, y-1, 0), Coord(x, y+1, 0), Coord(x+1, y, 0), Coord(x-1, y, 0))

    def adjacencies2: Set[Coord] = up ++ down ++ right ++ left
  }

  type Tiles = Set[Coord]
  val Empty = '.'
  val Bug   = '#'

  def showTiles(tiles: Tiles, level: Int): String = {
    def showRow(y: Int) =
      (0 to 4).foldLeft(new StringBuilder()) { case (sb, x) =>
        sb.append(
        if(tiles.contains(Coord(x, y, level))) Bug.toString
        else                                   Empty.toString)
      }.mkString

    (for {
       y <- 0 to 4
       r = showRow(y)
     } yield r).mkString("\n")
  }

  def parseRow(s: String, y: Int): Set[Coord] =
    s.toVector
      .zipWithIndex
      .foldLeft(Set.empty[Coord]) { case (accum, (c, i)) =>
        if(c == Bug) accum + Coord(i, y, 0)
        else         accum
      }

  def parseInput(input: Iterator[String]) =
    input
      .zipWithIndex
      .map { case (s, i) => parseRow(s, i) }
      .reduce(_ ++ _ )


  def readFile(f: String): Tiles =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day24.txt"

  val testData = """....#
                   |#..#.
                   |#..##
                   |..#..
                   |#....""".stripMargin
}
