package advent

import scala.collection.immutable.WrappedString

object Day04 {

  def run(): Unit = {
    val input = parseInput(puzzleInput)
    println(s"Day04.part1 = ${part1(input)}")
    println(s"Day04.part2 = ${part2(input)}")
  }

  def part1(input: List[Int]): Int =
    input
      .map(_.toString.toSeq)
      .filter(s => isIncreasing(s) && !partitionGroups(s).isEmpty)
      .size

  def part2(input: List[Int]): Int =
    input
      .map(_.toString.toSeq)
      .filter(s => isIncreasing(s) && !partitionGroups(s).filter(_.size == 2).isEmpty)
      .size

  def isIncreasing(s: WrappedString): Boolean = s == s.sorted

  def partitionGroups(s: WrappedString) = {
    def helper(accum: List[String], curr: String, rem: List[Char]): List[String] = {
      rem match {
        case Nil    if curr.size > 1 => curr +: accum                        // No more data, record group
        case Nil                     => accum                                // No more data
        case h :: t if curr.size == 0 => helper(accum, h.toString, t)         // No curr, advance one step
        case h :: t if curr.head == h => helper(accum, curr + h, t)           // Head matches current group
        case h :: t if curr.size > 1 => helper(curr +: accum, h.toString, t) // Head doesn't match current, record new group
        case h :: t                  => helper(accum, h.toString, t)         // Head doesn't match but current isn't a group
      }
    }

    helper(List.empty[String], "", s.toList)
  }

  val rangeRegex = """(\d+)-(\d+)""".r

  def parseInput(s: String): List[Int] = {
    val rangeRegex(start, end) = s
    (start.toInt to end.toInt).toList
  }

  val puzzleInput = "153517-630395"
}
