package advent

object Day18 {

  type VaultMap = Map[Coord, VaultObject]
  case class VaultState(pos: Coord, steps: Int, map: VaultMap) {
    def show: String = {
      s"Position: (${pos.x}, ${pos.y})  Steps:  $steps\n${showMap(map)}"
    }
  }

  sealed trait VaultObject
  case object Empty           extends VaultObject
  case object Wall            extends VaultObject
  case object Player          extends VaultObject
  case class Key(name: Char)  extends VaultObject
  case class Door(name: Char) extends VaultObject

  case class Coord(x: Int, y: Int) {
    def up    = Coord(x,   y-1)
    def down  = Coord(x,   y+1)
    def left  = Coord(x-1, y)
    def right = Coord(x+1, y)
  }


  def updateMap(map: VaultMap, pos: Coord, value: VaultObject): VaultMap =
    if(value == Empty) map - pos
    else              map.updated(pos, value)

  def initialState(map: VaultMap): VaultState = {
    val (player, _) = map.find { case (p, v) => v == Player }.get
    VaultState(player, 0, map)
  }

  def showMap(map: VaultMap): String = {
    val (Coord(minX, minY), Coord(maxX, maxY)) = extent(map)

    def showRow(y: Int): String =
      (minX to maxX).foldLeft(new StringBuilder()) { case (sb, x) =>
        map.get(Coord(x, y)) match {
          case Some(Wall)     => sb.append("#")
          case Some(Player) => sb.append("@")
          case Some(Key(n))   => sb.append(n.toString)
          case Some(Door(n))  => sb.append(n.toString)
          case _              => sb.append(".")
        }
      }.toString

    (for {
      y <- minY to maxY
      r =  showRow(y)
    } yield r).mkString("\n")
  }

  def extent(map: VaultMap): (Coord, Coord) = {
    val coords = map.keys
    (Coord(coords.minBy(_.x).x, coords.minBy(_.y).y), Coord(coords.maxBy(_.x).x, coords.maxBy(_.y).y))
  }


  def parseLine(y: Int, s: String): List[(Coord, VaultObject)] =
    LazyList.from(0).zip(s).foldLeft(List.empty[(Coord, VaultObject)]) { case (a, (x, o)) =>
      o match {
        case '#' => (Coord(x, y), Wall) +: a
        case '.' => a
        case '@' => (Coord(x, y), Player) +: a
        case c if c.isUpper => (Coord(x, y), Door(c)) +: a
        case c if c.isLower => (Coord(x, y), Key(c)) +: a
      }
    }

  def parseMap(input: Iterator[String]): VaultMap =
    LazyList.from(0)
      .zip(input)
      .flatMap { case (y, s) => parseLine(y, s) }
      .toMap

  def readFile(f: String): VaultMap =
    parseMap(io.Source.fromFile(f).getLines())



  val inputFile = "data/Day18.txt"

  val testData1 = """########################
                    |#f.D.E.e.C.b.A.@.a.B.c.#
                    |######################.#
                    |#d.....................#
                    |########################""".stripMargin.linesIterator
}
