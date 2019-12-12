package advent

object Day12 {

  def run(): Unit = {
    val moons = readFile(inputFile)
    println(s"Day12.part1 = ${part1(moons)}")
    println(s"Day12.part2 = ${part2(moons)}")
  }

  def part1(moons: Vector[Moon]): Int = {
    val state = (1 to 1000).foldLeft(moons) { case (ms, _) => updateMoons(ms) }
    totalEnergy(state)
  }

  def part2(moons: Vector[Moon]): Long =
    lcm(List(
        findCycle(moons.map(m =>  SimpleMoon(m.pos.x, m.vel.x))),
        findCycle(moons.map(m =>  SimpleMoon(m.pos.y, m.vel.y))),
        findCycle(moons.map(m =>  SimpleMoon(m.pos.z, m.vel.z)))))

  def lcm(ns: List[Int]): Long = {
    val l = ns.map(_.toLong).sorted.reverse
    val h = l.head
    val rest = l.tail

    def helper(n: Long): Long =
      if(!rest.exists(o => n%o != 0)) n
      else helper(n + h)

    helper(h)
  }

  case class SimpleMoon(c: Int, v: Int)

  def updateSimpleMoons(moons: Vector[SimpleMoon]): Vector[SimpleMoon] =
    moons.map(m => updateSimpleMoon(m, moons))

  def updateSimpleMoon(moon: SimpleMoon, state: Vector[SimpleMoon]): SimpleMoon = {
    val v = state.foldLeft(moon.v) { case (v, m) =>
      v + (if(moon.c < m.c) 1 else if(moon.c > m.c) -1 else 0)
    }

    SimpleMoon(moon.c + v, v)
  }

  def findCycle(initial: Vector[SimpleMoon]): Int = {
    def helper(n: Int, moons: Vector[SimpleMoon]): Int =
      if(moons == initial) n
      else                 helper(n+1, updateSimpleMoons(moons))

    helper(1, updateSimpleMoons(initial))
  }

  case class Triple(x: Int, y: Int, z: Int)

  case class Moon(pos: Triple, vel: Triple)

  val moonRegex = """<x=(-?\d+),\s*y=(-?\d+),\s*z=(-?\d+)>""".r

  def updateMoons(moons: Vector[Moon]): Vector[Moon] =
    moons.map(m => updateMoon(m, moons))

  def updateMoon(moon: Moon, state: Vector[Moon]): Moon = {
    val v = state.foldLeft(moon.vel) { case (Triple(vx, vy, vz), Moon(Triple(px, py, pz), _)) =>
      Triple(
        vx + (if(moon.pos.x < px) 1 else if(moon.pos.x > px) -1 else 0),
        vy + (if(moon.pos.y < py) 1 else if(moon.pos.y > py) -1 else 0),
        vz + (if(moon.pos.z < pz) 1 else if(moon.pos.z > pz) -1 else 0),
      )
    }

    Moon(Triple(moon.pos.x + v.x, moon.pos.y + v.y, moon.pos.z + v.z), v)
  }

  import Math._

  def computeEnergy(moon: Moon): Int =
    moon match { case Moon(Triple(x, y, z), Triple(vx, vy, vz)) =>
      (abs(x) + abs(y) + abs(z)) * (abs(vx) + abs(vy) + abs(vz))
    }

  def totalEnergy(moons: Vector[Moon]): Int =
    moons.map(computeEnergy).sum

  def parseMoon(s: String): Moon = {
    val moonRegex(x, y, z) = s
    Moon(Triple(x.toInt, y.toInt, z.toInt), Triple(0, 0, 0))
  }

  def parseMoons(i: Iterator[String]): Vector[Moon] =
    i.map(parseMoon).toVector

  def readFile(f: String): Vector[Moon] =
    io.Source.fromFile(f)
      .getLines()
      .map(parseMoon)
      .toVector

  val inputFile = "data/Day12.txt"

  val testData = "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"
  val testData2 = "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>"
}
