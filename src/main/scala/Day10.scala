package advent

object Day10 {

  def run(): Unit = {
    val cs = readFile(inputFile)
    println(s"Day10.part1 = ${part1(cs)}")
    println(s"Day10.part2 = ${part2(cs)}")
  }

  def part1(cs: List[Coord]) =
    cs.map(c => (c, computeSlopes(c, cs))).map { case (c, ss) => (c, ss.size) }.maxBy(_._2)

  def part2(cs: List[Coord]): Long = {
    val toRemove = computeRemoveOrder(cs)
    val c = toRemove.drop(199).head
    c.x*100 + c.y
  }

  def computeRemoveOrder(cs: List[Coord]): List[Coord] = {
    def helper(accum: List[Coord], slopes: Map[Ratio, List[Coord]], sorted: LazyList[Ratio]): List[Coord] =
      if(slopes.isEmpty) accum.reverse
      else
        slopes.get(sorted.head) match {
          case None           => helper(accum, slopes, sorted.tail)
          case Some(Nil)      => helper(accum, slopes - sorted.head, sorted.tail)
          case Some(h :: t)   => helper(h +: accum, slopes.updated(sorted.head, t), sorted.tail)
        }

    val (start, _) = part1(cs)

    val slopes = computeSlopes(start, cs)
    val sorted = LazyList.continually(sortSlopes(slopes.keys.toList)).flatten

    helper(List.empty[Coord], slopes, sorted)
  }

  def animate(cs: List[Coord]): Unit = {
    val (selected, _) = part1(cs)
    val remove = computeRemoveOrder(cs)

    val (tl, br) = extent(cs)

    print(AnsiCodes.EraseScreen)
    remove.foldLeft(cs) { case (coords, c) =>
      print(AnsiCodes.CursorHome)
      print(showMap(coords, selected, c, tl, br))
      Thread.sleep(10)
      coords.filterNot(_ == c)
    }

    ()
  }

  def sortSlopes(slopes: List[Ratio]) = {
    import Ordering.Double.IeeeOrdering
    val q1 = slopes.filter(r => r.n < 0 && r.d == 0) ++ slopes.filter(r => r.n < 0 && r.d > 0).map(r => (r, 1.0*r.n / r.d)).sortBy(_._2).map(_._1)
    val q2 = slopes.filter(r => r.n == 0 && r.d > 0) ++ slopes.filter(r => r.n > 0 && r.d > 0).map(r => (r, 1.0*r.n / r.d)).sortBy(_._2).map(_._1)
    val q3 = slopes.filter(r => r.n > 0 && r.d == 0) ++ slopes.filter(r => r.n > 0 && r.d < 0).map(r => (r, 1.0*r.n / r.d)).sortBy(_._2).map(_._1)
    val q4 = slopes.filter(r => r.n == 0 && r.d < 0) ++ slopes.filter(r => r.n < 0 && r.d < 0).map(r => (r, 1.0*r.n / r.d)).sortBy(_._2).map(_._1)

    q1 ++ q2 ++ q3 ++ q4
  }

  def computeSlopes(start: Coord, cs: List[Coord]) =
    cs
      .collect { case c if c != start => (slope(start, c) -> c) }
      .groupBy(_._1)
      .view.mapValues(_.map(_._2).sortBy(distance(start, _)))
      .toMap

  case class Coord(x: Long, y: Long)
  case class Ratio(n: Long, d: Long)

  def distance(c1: Coord, c2: Coord): Long =
    Math.abs(c2.x - c1.x) + Math.abs(c2.y - c1.y)

  def simplify(ratio: Ratio): Ratio = {
    val d = Math.abs(gcd(ratio.n, ratio.d))

    Ratio(ratio.n/d, ratio.d/d)
  }

  def normalize(rs: List[Ratio]): List[(Ratio, Ratio)] = {
    val nd = rs.map(_.d).distinct.product

    rs.map { r =>
      val x = nd / r.d
      (r, Ratio(r.n*x, r.d*x))
    }
  }

  def slope(a: Coord, b: Coord): Ratio = {
    val dx = b.x - a.x
    val dy = b.y - a.y

    simplify(Ratio(dy, dx))
  }

  def gcd(a: Long, b: Long): Long =
    if(b == 0) a
    else      gcd(b, a%b)


  def showMap(cs: List[Coord], selected: Coord, target: Coord, tl: Coord, br: Coord): String = {
    def showRow(y: Long): String =
      (tl.x to br.x).foldLeft(new StringBuilder()) { case (s, x) =>
        s.append(
          if(Coord(x, y) == selected) 'X'
          else if(Coord(x, y) == target) '*'
          else if(cs.contains(Coord(x, y))) '#'
          else '.')
      }.mkString

    (for {
      y <- tl.y to br.y
      r =  showRow(y)
    } yield r).mkString("\n")
  }

  def extent(cs: List[Coord]): (Coord, Coord) =
    (Coord(cs.minBy(_.x).x, cs.minBy(_.y).y), Coord(cs.maxBy(_.x).x, cs.maxBy(_.y).y))

  def parseLine(y: Int, s: String): Seq[Coord] =
    s.zipWithIndex.collect { case ('#', x) => Coord(x.toLong, y.toLong) }

  def readFile(f: String): List[Coord] =
    io.Source.fromFile(f)
      .getLines()
      .zipWithIndex
      .map { case (l, y) => parseLine(y, l) }
      .flatten
      .toList

  val inputFile = "data/Day10.txt"

  val test1 = "data/Day10-1.txt"
  val test2 = "data/Day10-2.txt"
  val test3 = "data/Day10-3.txt"
  val test4 = "data/Day10-4.txt"
  val test5 = "data/Day10-5.txt"
}

object AnsiCodes {
  // Device Status
  final val QueryDeviceCode = "\u001b[c"
  final val ReportDeviceCode = "\u001b[{code}0c"
  final val QueryDeviceStatus = "\u001b[5n"
  final val ReportDeviceOK = "\u001b[0n"
  final val ReportDeviceFailure = "\u001b[3n"
  final val QueryCursorPosition = "\u001b[6n"
//  final val ReportCursorPosition = """\u001b\[(\d+);(\d+)R""".r
  final val ReportCursorPosition = """.*\[(\d+);(\d+)R""".r

  // Terminal Setup
  final val ResetDevice = "\u001bc"
  final val EnableLineWrap = "\u001b[7h"
  final val DisableLineWrap = "\u001b[7l"

  // Cursor Control
  final def SetCursorPos(r: Int, c: Int) = s"\u001b[$r;${c}H"
  final val CursorHome = "\u001b[;H"
  final val CursorUp = "\u001b[{COUNT}A"
  final val CursorDown = "\u001b[{COUNT}B"
  final val CursorForward = "\u001b[{COUNT}C"
  final val CursorBackward = "\u001b[{COUNT}D"
  final val SaveCursor = "\u001b[s"
  final val UnsaveCursor = "\u001b[u"
  final val SaveCursorAttrs = "\u001b7"
  final val RestoreCursorAttrs = "\u001b8"

  // Erasing
  final val EraseEndofLine = "\u001b[K"
  final val EraseStartofLine = "\u001b[1K"
  final val EraseLine = "\u001b[2K"
  final val EraseDown = "\u001b[J"
  final val EraseUp = "\u001b[1J"
  final val EraseScreen = "\u001b[2J"
}
