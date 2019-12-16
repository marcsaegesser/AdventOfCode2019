package advent

import scala.math.Integral.Implicits._

object Day14 {

    def helper(rs: List[Reagent], reactions: Reactions): List[Reagent] = {
      val expand =
        rs.flatMap { r =>
          reactions.find { case (rhs, lhs) => rhs.name == r.name } match {
            case None             => List(r)
            case Some((rhs, lhs)) =>
              val factor = computeWholeFactor(r.amount, rhs.amount)
              lhs.map(l => l.copy(amount = l.amount*factor))
          }
        }
      val group = expand.groupBy(_.name)
      val next =
        group.map { case (n, rs) =>
          (n, Reagent(n, rs.map(_.amount).sum))
        }.values.toList

      next
    }

  def simplify(allReactions: Reactions) = {
    def helper(rs: List[Reagent], reactions: Reactions): List[Reagent] = {
      val expand =
        rs.flatMap { r =>
          reactions.find { case (rhs, lhs) => rhs.name == r.name } match {
            case None             => List(r)
            case Some((rhs, lhs)) =>
              val factor = computeWholeFactor(r.amount, rhs.amount)
              lhs.map(l => l.copy(amount = l.amount*factor))
          }
        }
      val group = expand.groupBy(_.name)
      val next =
        group.map { case (n, rs) =>
          (n, Reagent(n, rs.map(_.amount).sum))
        }.values.toList

      if(next != rs) helper(next, reactions)
      else          next
    }

    val otherReactions = allReactions.filterNot { case (k, v) => v.exists(_.name == "ORE") }
    val oreReactions = allReactions -- otherReactions.keys

    val nonOre = helper(List(Reagent("FUEL", 1)), otherReactions)

    helper(nonOre, oreReactions)
  }

  def computeWholeFactor(n: Int, d: Int): Int =
    (n /% d) match {
      case (q, 0) => q
      case (q, r) => q + 1
    }

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

  val testData3 = """2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
                    |17 NVRVD, 3 JNWZP => 8 VPVL
                    |53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
                    |22 VJHF, 37 MNCFX => 5 FWMGM
                    |139 ORE => 4 NVRVD
                    |144 ORE => 7 JNWZP
                    |5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
                    |5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
                    |145 ORE => 6 MNCFX
                    |1 NVRVD => 8 CXFTF
                    |1 VJHF, 6 MNCFX => 4 RFSQX
                    |176 ORE => 6 VJHF""".stripMargin

  val testData4 = """171 ORE => 8 CNZTR
                    |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
                    |114 ORE => 4 BHXH
                    |14 VRPVC => 6 BMBT
                    |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
                    |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
                    |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
                    |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
                    |5 BMBT => 4 WPTQ
                    |189 ORE => 9 KTJDG
                    |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
                    |12 VRPVC, 27 CNZTR => 2 XDBXC
                    |15 KTJDG, 12 BHXH => 5 XCVML
                    |3 BHXH, 2 VRPVC => 7 MZWV
                    |121 ORE => 7 VRPVC
                    |7 XCVML => 6 RJRHP
                    |5 BHXH, 4 VRPVC => 5 LTCX""".stripMargin
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

  val testData3 = """2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
                    |17 NVRVD, 3 JNWZP => 8 VPVL
                    |53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
                    |22 VJHF, 37 MNCFX => 5 FWMGM
                    |139 ORE => 4 NVRVD
                    |144 ORE => 7 JNWZP
                    |5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
                    |5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
                    |145 ORE => 6 MNCFX
                    |1 NVRVD => 8 CXFTF
                    |1 VJHF, 6 MNCFX => 4 RFSQX
                    |176 ORE => 6 VJHF""".stripMargin

  val testData4 = """171 ORE => 8 CNZTR
                    |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
                    |114 ORE => 4 BHXH
                    |14 VRPVC => 6 BMBT
                    |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
                    |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
                    |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
                    |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
                    |5 BMBT => 4 WPTQ
                    |189 ORE => 9 KTJDG
                    |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
                    |12 VRPVC, 27 CNZTR => 2 XDBXC
                    |15 KTJDG, 12 BHXH => 5 XCVML
                    |3 BHXH, 2 VRPVC => 7 MZWV
                    |121 ORE => 7 VRPVC
                    |7 XCVML => 6 RJRHP
                    |5 BHXH, 4 VRPVC => 5 LTCX""".stripMargin
}
