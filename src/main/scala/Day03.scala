package advent

import Math._

object Day03 {

  def run(): Unit = {
    val wires = readFile(inputFile)
    println(s"Day03.part1 = ${part1(wires)}")
    println(s"Day03.part2 = ${part2(wires)}")
  }

  def part1(wires: Wires): Int =
    findIntersections(wires)
      .map(_.distance(origin))
      .min

  def part2(wires: Wires): Int =
    findIntersectionsWithLengths(wires)
      .map(_._2)
      .min

  def findIntersections(w: Wires): List[Coord] = {
    (for {
      a <- w.p1
      b <- w.p2
      i = intersect(a, b)
    } yield i).flatten
  }

  def findIntersectionsWithLengths(w: Wires): List[(Coord, Int)] = {
    def helper(accum: List[(Coord, Int)], path1: Path, path2: Path, rem1: Path, rem2: Path): List[(Coord, Int)] = {
      (rem1, rem2) match {
        case (Nil, _) => accum
        case (h :: t, Nil) => helper(accum, h +: path1, emptyPath, t, w.p2)
        case (h1 :: t1, h2 :: t2) =>
          intersect(h1, h2) match {
            case Some(c) =>
              helper((c, pathLength(path1) + pathLength(path2) + h1.start.distance(c) + h2.start.distance(c)) +: accum,
                     h1 +: path1, emptyPath, t1, w.p2)
            case None    => helper(accum, path1, h2 +: path2, rem1, t2)
          }
      }
    }

    helper(List.empty[(Coord, Int)], emptyPath, emptyPath, w.p1, w.p2)
  }

  type Path = List[Segment]
  val emptyPath = List.empty[Segment]

  case class Wires(p1: Path, p2: Path)

  def pathLength(p: Path): Int =
    p.foldLeft(0) { case (l, s) => l + s.start.distance(s.end) }

  def intersect(s1: Segment, s2: Segment): Option[Coord] = {
    (isHorizontal(s1), isHorizontal(s2)) match {
      case (true, true) => None
      case (false, false) => None
      case (true, false) =>
        if(min(s1.start.x, s1.end.x) < s2.start.x && max(s1.start.x, s1.end.x) > s2.start.x && min(s2.start.y, s2.end.y) < s1.start.y && max(s2.start.y, s2.end.y) > s1.start.y)
          Some(Coord(s2.start.x, s1.start.y))
        else
          None
      case (false, true) =>
        if(min(s2.start.x, s2.end.x) < s1.start.x && max(s2.start.x, s2.end.x) > s1.start.x && min(s1.start.y, s1.end.y) < s2.start.y && max(s1.start.y, s1.end.y) > s2.start.y)
          Some(Coord(s1.start.x, s2.start.y))
        else
          None
    }
  }

  def isHorizontal(s: Segment): Boolean = s.start.y == s.end.y
  def isVertical(s: Segment): Boolean = s.start.x == s.end.x

  case class Coord(x: Int, y: Int) {
    def move(move: Move): Coord =
      move.dir match {
        case Right => Coord(x + move.length, y)
        case Left  => Coord(x - move.length, y)
        case Up    => Coord(x              , y + move.length)
        case Down  => Coord(x              , y - move.length)
      }

    def distance(from: Coord): Int =
      abs(x - from.x) + abs(y - from.y)
  }

  val origin = Coord(0, 0)

  case class Segment(start: Coord, end: Coord)

  sealed trait Direction
  case object Right extends Direction
  case object Left  extends Direction
  case object Up    extends Direction
  case object Down  extends Direction

  case class Move(dir: Direction, length: Int)

  val moveRegex = """([RLUD])(\d+)""".r

  def mkPath(moves: List[Move]): Path = {
    moves.foldLeft((origin, List.empty[Segment])) { case ((c, p), m) =>
      val end = c.move(m)
      val nextSeg = Segment(c, end)
      (end, nextSeg :: p)
    }._2.reverse
  }

  def parseMoves(input: String): List[Move] =
    moveRegex
      .findAllMatchIn(input)
      .map { m =>
        m.group(1) match {
          case "R" => Move(Right, m.group(2).toInt)
          case "L" => Move(Left, m.group(2).toInt)
          case "U" => Move(Up, m.group(2).toInt)
          case "D" => Move(Down, m.group(2).toInt)
        }
      }.toList

  def parseInput(lines: Iterator[String]): Wires = {
    val paths =
      lines
        .map(l => mkPath(parseMoves(l)))
        .toVector

    Wires(paths(0), paths(1))
  }

  def readFile(f: String) =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day03.txt"

  val testData1 = Iterator("R8,U5,L5,D3", "U7,R6,D4,L4")
  val testData2 = Iterator("R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83")
  val testData3 = Iterator("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
}
