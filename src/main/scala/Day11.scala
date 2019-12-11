package advent

object Day11 {
  import IntMachine._

  def run(): Unit = {
    val memory = readMemory(inputFile)

    println(s"Day1.part1 = ${part1(memory)}")
    println(s"Day1.part2 = \n${part2(memory)}")
  }

  def part1(memory: Memory): Int = {
    val state = runUntilHalted(State(Coord(0, 0), Up, Map.empty[Coord, Color], initializeMachine(memory, List())))
    state.hull.size
  }

  def part2(memory: Memory) = {
    val state = runUntilHalted(State(Coord(0, 0), Up, Map(Coord(0, 0) -> White), initializeMachine(memory, List())))
    showHull(state.hull)
  }

  def runUntilHalted(state: State): State =
    if(state.machine.state == Halted) state
    else                             runUntilHalted(runIteration(state))

  def runIteration(state: State): State = {
    state match { case State(p, d, h, m) =>
      val (out, next) = takeOutput(runMachine(provideInput(m, List(h.getOrElse(state.pos, Black)))))

      out match {
        case Nil         => state
        case h :: Nil    => throw new Exception("Invalid output")
        case c :: t :: _ => State(p.move(turn(d, t)), turn(d, t), h.updated(p, c), next)
      }
    }
  }

  type Color = Long
  val Black = 0L
  val White = 1L

  type Hull = Map[Coord, Color]

  case class State(pos: Coord, dir: Direction, hull: Hull, machine: Machine)
  def initialState(memory: Memory) = State(Coord(0, 0), Up, Map.empty[Coord, Color], initializeMachine(memory, List()))

  def extent(hull: Hull): (Coord, Coord) = {
    val coords = hull.keys
    val minX = coords.minBy(_.x).x
    val maxX = coords.maxBy(_.x).x
    val minY = coords.minBy(_.y).y
    val maxY = coords.maxBy(_.y).y

    (Coord(minX, minY), Coord(maxX, maxY))
  }

  def showColor(c: Color): Char =
    c match {
      case 0 => ' '
      case _ => '#'
    }

  def showHull(hull: Hull): String = {
    val (Coord(minX, minY), Coord(maxX, maxY)) = extent(hull)

    def showRow(y: Int): String =
      (minX to maxX).foldLeft(new StringBuilder()) {case (s, x) => s += showColor(hull.getOrElse(Coord(x, y), Black))}.mkString

    (for {
      y <- minY to maxY
      r = showRow(y)
    } yield r).mkString("\n")
  }

  case class Coord(x: Int, y: Int) {
    def move(d: Direction): Coord =
      d match {
        case Up    => Coord(x,   y-1)
        case Right => Coord(x+1, y)
        case Down  => Coord(x,   y+1)
        case Left  => Coord(x-1, y)
      }
  }

  type TurnDirection = Long
  val ToLeft = 0L
  val toRight = 1L


  sealed trait Direction
  case object Up    extends Direction
  case object Right extends Direction
  case object Down  extends Direction
  case object Left  extends Direction

  def turn(d: Direction, t: TurnDirection): Direction =
    d match {
      case Up    if t == ToLeft => Left
      case Up                  => Right
      case Right if t == ToLeft => Up
      case Right               => Down
      case Down  if t == ToLeft => Right
      case Down                => Left
      case Left  if t == ToLeft => Down
      case Left                => Up
    }

  val inputFile = "data/Day11.txt"
}
