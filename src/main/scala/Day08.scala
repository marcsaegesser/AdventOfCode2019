package advent

object Day08 {

  def run(): Unit = {
    val image = readFile(inputFile, inputHeight, inputWidth)
    println(s"Day08.part1 = ${part1(image)}")
    println(s"Day08.part2 = \n${part2(image)}")
  }

  def part1(image: Image): Int =
    image.layers
      .map { l => (l.filter(_=='0').size, l.filter(_=='1').size * l.filter(_=='2').size) }
      .sortBy(_._1)
      .head._2

  def part2(image: Image): String = {
    image.layers
      .transpose
      .map(_.filter(_ != '2').headOption.getOrElse('0'))
      .map { case '0' => ' '; case '1' => 'X'}
      .mkString
      .grouped(image.width)
      .mkString("\n")
  }
  case class Image(width: Int, height: Int, layers: Vector[Vector[Char]])

  def readFile(f: String, h: Int, w: Int): Image = {
    val layers = io.Source.fromFile(f)
      .getLines().flatten
      .grouped(h * w).map(_.toVector).toVector
    Image(w, h, layers)
  }

  val inputWidth = 25
  val inputHeight = 6
  val inputFile = "data/Day08.txt"
}
