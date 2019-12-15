package advent

object Day15 {
  import IntMachine._

  def run(): Unit = {
    val machine = initializeMachine(readMemory(inputFile), List())
    println(s"Day15.part1 = ${part1(machine)}")
    println(s"Day15.part2 = ${part2(machine)}")
  }

  def part1(machine: Machine): Int =
    searchTarget(machine)._1.path.size

  def part2(machine: Machine): Int = {
    val (state, _) = searchTarget(machine)
    val map = drawMap(machine)
    fillMap(map, state.pos)
  }

  def searchTarget(machine: Machine) = {
    def helper(active: List[DroidState], map: Map[Coord, Status]): (DroidState, Map[Coord, Status]) = {
      val next =
        active
          .flatMap(a => List((a, North, a.pos.move(North)), (a, South, a.pos.move(South)), (a, West, a.pos.move(West)), (a, East, a.pos.move(East))))
          .filterNot(x => map.contains(x._3))

      val (nextStates, newMap) =
        next.foldLeft((List.empty[DroidState], map)) { case ((ss, m), (state, d, coord)) =>
          val (status, mach) = stepDirection(state.machine, d)
            status match {
              case Wall   => (ss, m.updated(coord, Wall))
              case Step   => (DroidState(coord, d +: state.path, mach) +: ss, m.updated(coord, Step))
              case Target => (DroidState(coord, d +: state.path, mach) +: ss, m.updated(coord, Target))
            }
        }

      nextStates.find(s => newMap(s.pos) == Target) match {
        case None    => helper(nextStates, newMap)
        case Some(s) => (s, newMap)
      }
    }

    helper(List(DroidState(Coord(0, 0), List.empty[Direction], machine)), Map.empty[Coord, Status])
  }

  def drawMap(machine: Machine) = {
    def helper(depth: Int, active: List[DroidState], map: Map[Coord, Status]): Map[Coord, Status] = {
      val next =
        active
          .flatMap(a => List((a, North, a.pos.move(North)), (a, South, a.pos.move(South)), (a, West, a.pos.move(West)), (a, East, a.pos.move(East))))
          .filterNot(x => map.contains(x._3))

      val (nextStates, newMap) =
        next.foldLeft((List.empty[DroidState], map)) { case ((ss, m), (state, d, coord)) =>
          val (status, mach) = stepDirection(state.machine, d)
            status match {
              case Wall   => (ss, m.updated(coord, Wall))
              case Step   => (DroidState(coord, d +: state.path, mach) +: ss, m.updated(coord, Step))
              case Target => (DroidState(coord, d +: state.path, mach) +: ss, m.updated(coord, Target))
            }
        }

      nextStates match {
        case Nil => newMap
        case _   => helper(depth+1, nextStates, newMap)
      }
    }

    helper(0, List(DroidState(Coord(0, 0), List.empty[Direction], machine)), Map.empty[Coord, Status])
  }

  def fillMap(map: Map[Coord, Status], start: Coord): Int = {
    def helper(active: List[Coord], visited: Set[Coord], depth: Int): Int = {
      val next =
        active
          .flatMap(a => directions.map(d => a.move(d)) )
          .filterNot(visited.contains)
          .filter(a => map(a) != Wall)

      if(next.isEmpty) depth
      else             helper(next, visited ++ next, depth+1)
    }

    helper(List(start), Set(start), 0)
  }



  def stepDirection(machine: Machine, direction: Direction) = {
    val (o, m) = takeOutput(runMachine(provideInput(machine, List(direction))))
    (o.head, m)
  }

  def showMap(map: Map[Coord, Status]): String = {
    val (Coord(minX, minY), Coord(maxX, maxY)) = extent(map)
    def showRow(y: Int): String =
      (minX to maxX).foldLeft(new StringBuilder()) { case (s, x) =>
        s.append(
          map.get(Coord(x, y)) match {
            case None => " "
            case Some(Wall)   => "#"
            case Some(Step)   => "."
            case Some(Target) => "T"
            case Some(_)      => " "
        })
      }.mkString

    (for {
      y <- (minY to maxY)
      r =  showRow(y)
    } yield r).mkString("\n")

  }

  def extent(map: Map[Coord, Status]): (Coord,  Coord) =
    (Coord(map.keys.minBy(_.x).x, map.keys.minBy(_.y).y),
      Coord(map.keys.maxBy(_.x).x, map.keys.maxBy(_.y).y))

  case class DroidState(pos: Coord, path: List[Direction], machine: Machine)

  case class Coord(x: Int, y: Int) {
    def move(d: Direction): Coord =
      d match {
        case North => Coord(x,   y+1)
        case South => Coord(x,   y-1)
        case West  => Coord(x-1, y)
        case East  => Coord(x+1, y)
      }
  }

 type Direction = Long
  val North = 1L
  val South = 2L
  val West  = 3L
  val East  = 4L
  val directions = List(North, South, East, West)

  type Status = Long
  val Wall   = 0L
  val Step   = 1L
  val Target = 2L

  val inputFile = "data/Day15.txt"
}
