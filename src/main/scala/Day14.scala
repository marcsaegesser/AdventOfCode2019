package advent

import scala.math.Integral.Implicits._

object Day14 {

  def simplify(reactions: Reactions) = {
    def helper(rs: List[Reagent], avail: Map[String, Int]): Map[String, Int] =
      rs match {
        case Nil    => avail
        case Reagent(name, amount) :: t =>
          reactions.find { case (r, rs) => r.name == name } match {
            case None => helper(t, updateAvailable(avail, name, amount)) // No more simpflications
            case Some((r, rs)) =>
              val wholeAmount = computeWholeAmount(amount, r.amount)
              val factor = wholeAmount / r.amount
              val extra = wholeAmount - r.amount
              val newReagents = rs.map(reagent => reagent.copy(amount = reagent.amount * factor))
              helper(newReagents ++ t, updateAvailable(avail, r.name, extra))
          }
      }

    helper(List(Reagent("FUEL", 1)), Map.empty[String, Int])
  }

  def updateAvailable(avail: Map[String , Int], name: String, amount: Int): Map[String, Int] =
    avail.updated(name, avail.getOrElse(name, 0) + amount)

  def computeWholeAmount(n: Int, d: Int): Int =
    (n /% d) match {
      case (q, 0) => n
      case (q, r) => n + d - r
    }

  // def computeWholeAmount(name: String, need: Int, reactions: Reactions): List[Reagent] = {
  //   reactions.find { case (r, rs) => r.name == name } match {
  //     case None => List()
  //     case Some((r, rs)) => rs.map { case Reagent(n, a) => Reagent() }
  //   }
  // }

  // def wholeAmount(need: Int, numerator: Int, denominator: Int): Int =
  //   (need*numerator) /% denominator match {
  //     case (q, 0) => q
  //     case (q, _) => ???
  //   }

  // def computeWholeAmount(reactions: Reactions, name: String, need: Int): Int = {
  //   val Rational(n, d) = oreReactions(name).head.amount.scale(need)

  //   n /% d match {
  //     case (q, 0) => q
  //     case (q, _) => q + d
  //   }
  // }


  // def simplifyReagent(reagent: Reagent, reactions: Reactions): List[Reagent] = {
  //   reactions.get(reagent.name) match {
  //     case None     => List(reagent)
  //     case Some(rs) => rs.flatMap(r => simplifyReagent(r, reactions)).map(r => scaleReagent(r, reagent.amount))
  //   }
  // }

  case class Reagent(name: String, amount: Int)

  type Reactions = Map[Reagent, List[Reagent]]


  val equationRegex = """(.+)\s+=>\s+(.+)""".r
  val reagentRegex = """(\d+)\s(\w+)""".r

  def parseReaction(s: String): (Reagent, List[Reagent]) = {
    val equationRegex(lhs, rhs) = s
    val inputs = reagentRegex.findAllMatchIn(lhs)
    val output = reagentRegex.findAllMatchIn(rhs)

    val out = output.map(m => Reagent(m.group(2), m.group(1).toInt)).toList.head
    val in = inputs.map(m => Reagent(m.group(2), m.group(1).toInt)).toList
    (out, in)
  }

  def parseInput(lines: Iterator[String]): Reactions =
    lines.map(parseReaction).toMap

  def readFile(f: String): Reactions =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day14.txt"

  val testData1 = """9 ORE => 2 A
                    |8 ORE => 3 B
                    |7 ORE => 5 C
                    |3 A, 4 B => 1 AB
                    |5 B, 7 C => 1 BC
                    |4 C, 1 A => 1 CA
                    |2 AB, 3 BC, 4 CA => 1 FUEL""".stripMargin

  val testData2 = """157 ORE => 5 NZVS
                    |165 ORE => 6 DCFZ
                    |44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
                    |12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
                    |179 ORE => 7 PSHF
                    |177 ORE => 5 HKGWZ
                    |7 DCFZ, 7 PSHF => 2 XJWVT
                    |165 ORE => 2 GPVTF
                    |3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT""".stripMargin
}

object Day14X {

  def part1(reactions: Reactions) = {
    val ores = simplifyReagent(Reagent(Rational(1, 1), "FUEL"), reactions)

    ores
      .map(_.amount).groupBy(_.d)
      .map { case (k, v) => (k, v.reduce(_+_)) }
      .toList
      .map { case (x, r) => Rational(r.n, r.d*x).ceil * x }
      .sum
  }

  def fubar(reactions: Reactions) = {
    val mainReactions = reactions.filterNot { case (k, v) => v.exists(_.name == "ORE")}
    val oreReactions = reactions.filter { case (k, v) => v.exists(_.name == "ORE")}

    val basics = simplifyReagent(Reagent(Rational(1, 1), "FUEL"), mainReactions)

    basics
      .groupBy(_.name)
      .map { case (k, v) => (k, v.map(_.amount).reduce(_+_).ceil) }
      .map { case (k, v) => (k, computeWholeAmount(oreReactions, k, v)) }
      .values.sum
  }

  def computeWholeAmount(oreReactions: Reactions, name: String, need: Int): Int = {
    val Rational(n, d) = oreReactions(name).head.amount.scale(need)

    n /% d match {
      case (q, 0) => q
      case (q, _) => q + d
    }
  }

  def computeWholeAmount(oreReactions: Reactions, name: String, need: Rational): Int = {
    val Rational(n, d) = oreReactions(name).head.amount * need

    n /% d match {
      case (q, 0) => q
      case (q, _) => q + d
    }
  }


  def simplifyReagent(reagent: Reagent, reactions: Reactions): List[Reagent] = {
    reactions.get(reagent.name) match {
      case None     => List(reagent)
      case Some(rs) => rs.flatMap(r => simplifyReagent(r, reactions)).map(r => scaleReagent(r, reagent.amount))
    }
  }

  def scaleReagent(r: Reagent, n: Int): Reagent =
    Reagent(Rational(r.amount.n*n, r.amount.d), r.name)

  def scaleReagent(r: Reagent, s: Rational): Reagent =
    Reagent(r.amount * s, r.name)

  case class Rational(n: Int, d: Int) {
    def gcd(a: Int, b: Int): Int =
      if(b == 0) a
      else      gcd(b, a%b)

    def +(other: Rational): Rational =
      Rational(other.n*d + n*other.d, other.d*d).simplify

    def *(other: Rational): Rational =
      Rational(n*other.n, d*other.d).simplify

    def scale(x: Int): Rational = Rational(n*x, d)

    def ceil: Int = {
      n /% d match {
        case (q, 0) => q
        case (q, r) => q + 1
      }
    }

    def simplify: Rational = {
      val c = Math.abs(gcd(n, d))

      Rational(n/c, d/c)
    }
  }

  case class Reagent(amount: Rational, name: String)
  case class Reaction(inputs: List[Reagent], output: Reagent)

  type Reactions = Map[String, List[Reagent]]

  val equationRegex = """(.+)\s+=>\s+(.+)""".r
  val reagentRegex = """(\d+)\s(\w+)""".r

  def parseReaction(s: String): (String, List[Reagent]) = {
    val equationRegex(lhs, rhs) = s
    val inputs = reagentRegex.findAllMatchIn(lhs)
    val output = reagentRegex.findAllMatchIn(rhs)

    val out = output.map(m => Reagent(Rational(m.group(1).toInt, 1), m.group(2))).toList.head

    (out.name, inputs.map(m => Reagent(Rational(m.group(1).toInt, out.amount.n), m.group(2))).toList)
  }

  def parseInput(lines: Iterator[String]): Reactions =
    lines.map(parseReaction).toMap

  def readFile(f: String): Reactions =
    parseInput(io.Source.fromFile(f).getLines())

  val inputFile = "data/Day14.txt"

  val testData1 = """9 ORE => 2 A
                    |8 ORE => 3 B
                    |7 ORE => 5 C
                    |3 A, 4 B => 1 AB
                    |5 B, 7 C => 1 BC
                    |4 C, 1 A => 1 CA
                    |2 AB, 3 BC, 4 CA => 1 FUEL""".stripMargin

  val testData2 = """157 ORE => 5 NZVS
                    |165 ORE => 6 DCFZ
                    |44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
                    |12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
                    |179 ORE => 7 PSHF
                    |177 ORE => 5 HKGWZ
                    |7 DCFZ, 7 PSHF => 2 XJWVT
                    |165 ORE => 2 GPVTF
                    |3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT""".stripMargin
}
