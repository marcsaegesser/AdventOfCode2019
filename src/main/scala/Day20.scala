package advent

object Day20 {

  def run(): Unit = {
    val maze = parseMaze(readFile(inputFile))

    println(s"Day20.part1 = ${shortestPath(maze)}")
    println(s"Day20.part2 = ${shortestPath2(maze)}")
  }

  def shortestPath(maze: Maze): Int = {
    def helper(active: Set[Coord], end: Coord, n: Int, visited: Set[Coord]): Int = {
      if(active.contains(end)) n  // We've found the end
      else {
        // Find all unvisited tiles adjacent to active region
        val nextVisited = visited ++ active
        val next = active.flatMap(c => adjacentTiles(maze, c)) -- nextVisited
        helper(next, end, n+1, nextVisited)
      }
    }

    val start = findStart(maze).map(_.coord).toSet
    val end = findEnd(maze).map(_.coord).get
    helper(start, end, 0, Set.empty[Coord])
  }

  def shortestPath2(maze: Maze): Int = {
    def helper(active: Set[Position], end: Position, n: Int, visited: Set[Position]): Int = {
      if(active.contains(end)) n  // We've found the end
      else {
        // Find all unvisited tiles adjacent to active region
        val nextVisited = visited ++ active
        val next = active.flatMap(p => adjacentPositions(maze, p)) -- nextVisited
        helper(next, end, n+1, nextVisited)
      }
    }

    helper(findStart(maze).toSet, findEnd(maze).get, 0, Set.empty[Position])
  }

  def adjacentTiles(maze: Maze, coord: Coord): Set[Coord] = {
    List(
      tileAt(maze, coord.up),
      tileAt(maze, coord.right),
      tileAt(maze, coord.down),
      tileAt(maze, coord.left)
    ).flatten.toSet
  }

  def tileAt(maze: Maze, coord: Coord): Option[Coord] =
      maze.get(coord) match {
        case Some(Tile)               => Some(coord)
        case Some(Transport(n, _, _)) => findTransportDest(maze, n, coord)
        case None                     => None
      }

  def adjacentPositions(maze: Maze, position: Position): Set[Position] = {
    List(
      tileAt(maze, position.up),
      tileAt(maze, position.right),
      tileAt(maze, position.down),
      tileAt(maze, position.left)
    ).flatten.toSet
  }

  def tileAt(maze: Maze, position: Position): Option[Position] =
      maze.get(position.coord) match {
        case Some(Tile) => Some(position)
        case Some(Transport(n, _, true)) if position.level > 0 =>
          findTransportDest(maze, n, position.coord).map(c => Position(c, position.level-1))
        case Some(Transport(n, _, true))  => None
        case Some(Transport(n, _, false)) =>
          findTransportDest(maze, n, position.coord).map(c => Position(c, position.level+1))
        case None                  => None
      }

  sealed trait  MazeObject
  case   object Tile                                                  extends MazeObject
  case   class  Transport(name: String, coord: Coord, outer: Boolean) extends MazeObject

  case class Coord(x: Int, y: Int) {
    def up    = Coord(x,   y-1)
    def down  = Coord(x,   y+1)
    def left  = Coord(x-1, y)
    def right = Coord(x+1, y)
  }

  case class Position(coord: Coord, level: Int) {
    def up    = Position(coord.up, level)
    def down  = Position(coord.down, level)
    def left  = Position(coord.left, level)
    def right = Position(coord.right, level)
  }

  type Maze = Map[Coord, MazeObject]

  def emptyMap = Map.empty[Coord, MazeObject]

  def findStart(maze: Maze): Option[Position] =
    maze.collectFirst { case (_, Transport("AA", c, _))  => Position(c, 0) }

  def findEnd(maze: Maze): Option[Position] =
    maze.collectFirst { case (_, Transport("ZZ", c, _))  => Position(c, 0) }

  def findTransportDest(maze: Maze, name: String, coord: Coord): Option[Coord] =
    maze.collect {
      case (c, Transport(n, dc, _)) if n == name && c != coord => dc }.headOption


  def parseMaze(input: List[String]): Maze = {
    def isOuter(line: String, coord: Coord): Boolean = {
      val len = line.size
      (coord.x <= 1 || coord.x >= len-1) || (coord.y <= 1 || coord.y >= input.size-1)
    }

    def parseLine(maze: Maze, teleports: Map[Coord, Char], n: Int, line: String): (Maze, Map[Coord, Char]) =
      line.zipWithIndex
        .foldLeft((maze, teleports)) { case ((m, ts), (c, i)) =>
          c match {
            case '#' => (m, ts)                          // Don't record walls
            case ' ' => (m, ts)                          // Don't record empty spaces
            case '.' => (m + (Coord(i, n) -> Tile), ts)  // An open tile
            case chr =>                                  // A character that might be the start or end of a transport
              val coord = Coord(i, n)
              val outer = isOuter(line, coord)
              (ts.get(coord.up), m.get(coord.up.up), ts.get(coord.left), m.get(coord.left.left)) match {
                case (Some(ch), Some(_), _,        _)       => (m + (coord.up   -> Transport(s"$ch$chr", coord.up.up, outer)), ts - coord) // Vertical tile above
                case (Some(ch), None,    _,        _)       => (m + (coord      -> Transport(s"$ch$chr", coord.down, outer)), ts - coord) // Vertical tile down
                case (None,     _,       Some(ch), Some(_)) => (m + (coord.left -> Transport(s"$ch$chr", coord.left.left, outer)), ts - coord) // Horizontal tile left
                case (None,     _,       Some(ch), None)    => (m + (coord      -> Transport(s"$ch$chr", coord.right, outer)), ts - coord) // Horizontal tile right
                case (None,     _,       None,     _)       => (m, ts + (coord -> chr))  // First char of transport name
              }
          }
        }

    def helper(maze: Maze, teleports: Map[Coord, Char], n: Int, lines: List[String]): Maze = {
      lines match {
        case Nil     => maze
        case l :: ls =>
          val (m, ts) = parseLine(maze, teleports, n, l)
          helper(m, ts, n+1, ls)
      }
    }

    helper(emptyMap, Map.empty[Coord, Char], 0, input)
  }

  def readFile(f: String): List[String] =
    io.Source.fromFile(f)
      .getLines()
      .toList

  val inputFile = "data/Day20.txt"
  val testData1 = "data/Day20-small.txt"
  val testData2 = "data/Day20-test2.txt"
  val testData3 = "data/Day20-test3.txt"
}
