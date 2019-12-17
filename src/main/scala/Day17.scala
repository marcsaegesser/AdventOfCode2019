package advent

object Day17 {
  import IntMachine._

  def run(): Unit = {
    val memory = readMemory(inputFile)
    println(s"Day17.part1 = ${part1(memory)}")
    println(s"Day17.part2 = ${part2(memory)}")
  }

  def part1(memory: Memory): Int = {
    val (o, _) = takeOutput(runMachine(initializeMachine(memory, List())))
    findIntersections(parseInput(o)).map { case Coord(x, y) => x*y }.sum
  }

  def part2(memory: Memory) = {
    // By observation of the map and lots of thinking
    val mainRoutine = "A,B,A,C,A,B,C,A,B,C\n".map(_.toLong)
    val funcA = "R,12,R,4,R,10,R,12\n".map(_.toLong)
    val funcB = "R,6,L,8,R,10\n".map(_.toLong)
    val funcC = "L,8,R,4,R,4,R,6\n".map(_.toLong)
    val disp = "n\n".map(_.toLong)

    val m = initializeMachine(memory, List(mainRoutine, funcA, funcB, funcC, disp).flatten)
    val (o, _) = takeOutput(runMachine(setMemory(m, 0, 2)))

    o.last
  }

  def findIntersections(state: RobotState): List[Coord] =
    state.map.toList.filter(_._2 == Scaffold).foldLeft(List.empty[Coord]) { case (a, (c, v)) =>
      (state.map.get(c.up), state.map.get(c.down), state.map.get(c.left), state.map.get(c.right)) match {
        case (Some(_), Some(_), Some(_), Some(_)) => c +: a
        case _                                    => a
      }
    }
  case class Coord(x: Int, y: Int) {
    def up    = Coord(x,   y-1)
    def down  = Coord(x,   y+1)
    def left  = Coord(x-1, y)
    def right = Coord(x+1, y)
  }

  sealed trait Direction
  case object Up    extends Direction
  case object Down  extends Direction
  case object Left  extends Direction
  case object Right extends Direction

 type CameraMap = Map[Coord, Char]
  case class RobotState(map: CameraMap, pos: Coord, dir: Direction)

  val Scaffold = '#'
  val RobotUp = '^'
  val RobotDown = 'v'
  val RobotLeft = '<'
  val RobotRight = '>'
  val Empty    = '.'

  def parseInput(input: List[Long]): RobotState = {
    val (map, rc, rd, _) =
      input.map(_.toChar).foldLeft((List.empty[(Coord, Char)], Coord(0,0), Up: Direction, Coord(0, 0))) { case ((map, rc, rd, c), i) =>
        i match {
          case Scaffold    => ((c, Scaffold) +: map, rc, rd, Coord(c.x+1, c.y))
          case Empty       => (map, rc, rd, Coord(c.x+1, c.y))
          case RobotUp     => ((c, Scaffold) +: map, c, Up, Coord(c.x+1, c.y))
          case RobotDown   => ((c, Scaffold) +: map, c, Down, Coord(c.x+1, c.y))
          case RobotLeft   => ((c, Scaffold) +: map, c, Left, Coord(c.x+1, c.y))
          case RobotRight  => ((c, Scaffold) +: map, c, Right, Coord(c.x+1, c.y))
          case '\n'        => (map, rc, rd, Coord(0, c.y+1))
          case _           => throw new Exception("Bang!")
        }
      }

    RobotState(map.toMap, rc, rd)
  }

  val inputFile = "data/Day17.txt"
}
