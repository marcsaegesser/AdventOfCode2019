package advent

object Day18 {
  type VaultMap = Map[Coord, VaultObject]

  case class Path(path: List[Char], length: Int)

  case class State(map: VaultMap, pos: Coord, path: Path, solution: Option[Path]) {
    override def toString = s"State($pos, $path, $solution)"
  }

  def traverse(state: State): Option[Path] = {
    println(state)
    val keys = reachableKeys(state.map, state.pos)
    println(s"keys=$keys")
    keys match {
      case Nil => Some(state.path)
      case ks  =>
        ks.foldLeft(state.solution) { case (s, (c, l)) =>
          s match {
            case Some(p) if (state.path.length+l) < p.length => traverse(updateState(state, c, l, s))
            case Some(p) => s
            case None    => traverse(updateState(state, c, l, s))
          }
        }
    }
  }

  def initialState(map: VaultMap): State = {
    val (player, _) = map.find {
      case (p, Player) => true
      case (p, _)      => false
    }.get
    State(map, player, Path(List.empty[Char], 0), Option.empty[Path])
  }

  def updateState(state: State, pos: Coord, l: Int, solution: Option[Path]) = {
    val (newMap, k) = takeKey(state.map, state.pos, pos)
    val newPath = Path(k +: state.path.path, state.path.length + l)
    State(newMap, pos, newPath, solution)
  }

  def takeKey(map: VaultMap, currPos: Coord, keyPos: Coord): (VaultMap, Char) = {
      map.get(keyPos) match {
        case Some(Key(n)) => (updateMap(updateMap(removeDoor(map, n.toUpper), keyPos, Player), currPos, Empty), n)
        case _            => throw new Exception("Invalid state")
      }
  }

  def removeDoor(map: VaultMap, name: Char): VaultMap =
    map.find { case (c, o) => o == Door(name) } match {
      case None         => map
      case Some((c, _)) => updateMap(map, c, Empty)
    }

  def updateMap(map: VaultMap, pos: Coord, value: VaultObject): VaultMap =
    if(value == Empty) map - pos
    else               map.updated(pos, value)

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

  def reachableKeys(map: VaultMap, pos: Coord): List[(Coord, Int)] = {
    def helper(found: List[(Coord, Int)], depth: Int, visited: Set[Coord], active: Set[Coord]): List[(Coord, Int)] = {
      if(active.isEmpty)             found
      else if(!keyExists(map)) found
      else {
        val (f, n) = active.partition(c => isKey(map, c))
        val nextFound = f.map(c => (c, depth)).toList ++ found
        val nextCoords = n.flatMap(c => reachablePositions(map, c)) -- visited

        helper(nextFound, depth+1, visited ++ f ++ n, nextCoords)
      }
    }

    helper(List(), 0, Set(), Set(pos))
  }

  def reachablePositions(map: VaultMap, pos: Coord): Set[Coord] =
    Set(pos.up, pos.down, pos.left, pos.right).map(c => (c, map.get(c))).collect {
      case (c, None)         => c
      case (c, Some(Key(_))) => c
    }

  def keyExists(map: VaultMap): Boolean =
    map.exists {
      case (k, Key(_)) => true
      case (k, _)      => false
    }

  def isKey(map: VaultMap, pos: Coord): Boolean =
    map.get(pos) match {
      case Some(Key(_)) => true
      case _              => false
    }


  def showMap(map: VaultMap): String = {
    val (Coord(minX, minY), Coord(maxX, maxY)) = extent(map)

    def showRow(y: Int): String =
      (minX to maxX).foldLeft(new StringBuilder()) { case (sb, x) =>
        map.get(Coord(x, y)) match {
          case Some(Wall)    => sb.append("#")
          case Some(Player)  => sb.append("@")
          case Some(Key(n))  => sb.append(n.toString)
          case Some(Door(n)) => sb.append(n.toString)
          case _             => sb.append(".")
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
                    |########################""".stripMargin

  val testData2 = """#################
                    |#i.G..c...e..H.p#
                    |########.########
                    |#j.A..b...f..D.o#
                    |########@########
                    |#k.E..a...g..B.n#
                    |########.########
                    |#l.F..d...h..C.m#
                    |#################""".stripMargin

  val testData3 = """########################
                    |#@..............ac.GI.b#
                    |###d#e#f################
                    |###A#B#C################
                    |###g#h#i################
                    |########################""".stripMargin

}

object Day18a {

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

  def findMin(state: VaultState) = {
    def helper(found: Option[VaultState], active: List[VaultState]): Option[VaultState] = {
      val maxSteps = found.map(_.steps).getOrElse(Int.MaxValue)

      active.sortBy(_.steps) match {
        case Nil    => found
        case h :: t if h.steps < maxSteps =>
          // First search the tree based on h
          val first =
            reachableKeys(h) match {
              case Nil => if(h.steps < maxSteps) Some(h) else found // No more keys, h is a solution
              case l    => helper(found, l)                         // Descend on h's chidren
            }
          val rest = helper(first, t)
          (first, rest) match {
            case (None, None) => None
            case (Some(a), None) => first
            case (None, Some(b)) => rest
            case (Some(a), Some(b)) if a.steps < b.steps => first
            case _                                       => rest
          }
        case h :: t => helper(found, t)   // Head is already too many steps
      }
    }

    helper(None, List(state))
  }

  def reachableKeys(state: VaultState): List[VaultState] = {
    def helper(found: List[VaultState], depth: Int, visited: Set[Coord], active: Set[Coord]): List[VaultState] = {
      if(active.isEmpty)             found
      else if(!keyExists(state.map)) found
      else {
        val (f, n) = active.partition(c => isKey(state.map, c))
        val nextFound = f.map(c => takeKey(c, depth, state)).toList ++ found
        val nextCoords = n.flatMap(c => reachablePositions(state.map, c)) -- visited

        helper(nextFound, depth+1, visited ++ f ++ n, nextCoords)
      }
    }

    helper(List(), 0, Set(), Set(state.pos))
  }

  def reachablePositions(map: VaultMap, pos: Coord): Set[Coord] =
    Set(pos.up, pos.down, pos.left, pos.right).map(c => (c, map.get(c))).collect {
      case (c, None)         => c
      case (c, Some(Key(_))) => c
    }

  def keyExists(map: VaultMap): Boolean =
    map.exists {
      case (k, Key(_)) => true
      case (k, _)      => false
    }

  def isKey(map: VaultMap, pos: Coord): Boolean =
    map.get(pos) match {
      case Some(Key(_)) => true
      case _              => false
    }

  def takeKey(pos: Coord, steps: Int, state: VaultState): VaultState = {
    val map =
      state.map.get(pos) match {
        case Some(Key(n)) => updateMap(updateMap(removeDoor(state.map, n.toUpper), pos, Player), state.pos, Empty)
        case _            => throw new Exception("Invalid state")
      }

    VaultState(pos, state.steps + steps, map)
  }

  def removeDoor(map: VaultMap, name: Char): VaultMap =
    map.find { case (c, o) => o == Door(name) } match {
      case None         => map
      case Some((c, _)) => updateMap(map, c, Empty)
    }

  def updateMap(map: VaultMap, pos: Coord, value: VaultObject): VaultMap =
    if(value == Empty) map - pos
    else               map.updated(pos, value)

  def initialState(map: VaultMap): VaultState = {
    val (player, _) = map.find {
      case (p, Player) => true
      case (p, _)      => false
    }.get
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
                    |########################""".stripMargin

  val testData2 = """#################
                    |#i.G..c...e..H.p#
                    |########.########
                    |#j.A..b...f..D.o#
                    |########@########
                    |#k.E..a...g..B.n#
                    |########.########
                    |#l.F..d...h..C.m#
                    |#################""".stripMargin

  val testData3 = """########################
                    |#@..............ac.GI.b#
                    |###d#e#f################
                    |###A#B#C################
                    |###g#h#i################
                    |########################""".stripMargin
}