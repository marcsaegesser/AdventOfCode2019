package advent

object Day06 {

  def run(): Unit = {
    val tree = buildTreeForName("COM", readFile(inputFile))
    println(s"Day06.part1 = ${part1(tree)}")
    println(s"Day06.part2 = ${part2(tree)}")
  }

  def part1(tree: Tree[String]): Int = sumDepths(tree, 0)

  def part2(tree: Tree[String]): Int = {
    val (l1, l2) = dropCommonPrefix(findPath(tree, "YOU").get, findPath(tree, "SAN").get)
    l1.size + l2.size
  }

  case class Tree[A](val value: A, children: List[Tree[A]] = List.empty[Tree[A]])

  def sumDepths(tree:  Tree[String], depth: Int): Int =
    depth + tree.children.map(c => sumDepths(c, depth+1)).sum

  def findPath(tree: Tree[String], target: String): Option[List[String]] = {
    def helper(path: List[String], trees: List[Tree[String]]): Option[List[String]] =
      trees match {
        case Nil                         => None
        case h :: t if h.value == target => Some(path.reverse)
        case h :: t                      => helper(h.value +: path, h.children) orElse helper(path, t)
      }

    helper(List.empty[String], List(tree))
  }

  def dropCommonPrefix(l1: List[String], l2: List[String]): (List[String], List[String]) =
    (l1, l2) match {
      case (Nil, _)                        => ???
      case (_, Nil)                        => ???
      case (h1 :: t1, h2 :: t2) if h1 == h2 => dropCommonPrefix(t1, t2)
      case _                               => (l1, l2)
    }

  def buildTreeForName(name: String, data: Map[String, List[String]]): Tree[String] =
    data.get(name) match {
      case Some(cs) => Tree(name, cs.map(n => buildTreeForName(n, data)))
      case None     => Tree(name)
    }


  def readFile(f: String): Map[String, List[String]] =
    parseMap(io.Source.fromFile(f).getLines())

  val entryRegex = """(.+)\)(.+)""".r

  def parseMap(entries: Iterator[String]): Map[String, List[String]] = {
    entries.foldLeft(List.empty[(String, String)]) { case (a, e) =>
      val entryRegex(orbits, name) = e
      (orbits, name) +: a
    }.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
  }

  val inputFile = "data/Day06.txt"
  val testFile = "data/Day06-small.txt"
  val testFile2 = "data/Day06-small2.txt"
}
