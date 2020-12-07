import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.io.Source

// https://adventofcode.com/2020/day/7
class DaySevenSpec extends AnyFlatSpec with Matchers {

  val baseCase =
    """|light red bags contain 1 bright white bag, 2 muted yellow bags.
       |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
       |bright white bags contain 1 shiny gold bag.
       |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
       |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
       |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
       |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
       |faded blue bags contain no other bags.
       |dotted black bags contain no other bags.""".stripMargin
      .split("\n")
      .toSeq

  val in = Source
    .fromResource("7.txt")
    .getLines()
    .toSeq

  type Count = Int
  type Color = String
  type Rules = Map[Color, List[(Color, Count)]]

  val ruleRe = "([a-z]+ [a-z]+) bags contain (.+).".r
  val containRe = "(\\d) ([a-z]+ [a-z]+) bags?".r

  def parseRules(lines: Seq[String]): Rules =
    lines.map {
      case ruleRe(color, contain) =>
        (
          color,
          contain
            .split(",")
            .map(_.strip)
            .flatMap {
              case containRe(count, color) => Some((color, count.toInt))
              case _                       => None
            }
            .toList
        )
      case rule => throw new RuntimeException(s"Parsing error: $rule")
    }.toMap

  def findContainers(color: Color, rules: Rules): Set[Color] = {
    @tailrec
    def canHold(colors: Seq[Color], found: Seq[Color]): Seq[Color] =
      colors match {
        case Nil => found
        case color :: tail =>
          val newFound = rules
            .filter {
              case (_, colors) =>
                colors.exists {
                  case (c, _) => c == color
                }
            }
            .keys
            .toList
            .filterNot(found.contains(_))

          canHold(tail ++ newFound, found ++ newFound)
      }

    canHold(List(color), Nil).toSet
  }

  def countBags(color: Color, rules: Rules): Int = {
    @tailrec
    def add(colors: List[(Color, Count)], total: Int): Int =
      colors match {
        case Nil => total
        case (color, count) :: tail =>
          add(
            tail ++ rules.getOrElse(color, Nil).map {
              case (col, cnt) => (col, cnt * count)
            },
            total + count
          )
      }

    add(rules(color), 0)
  }

  "parseRules" should "parse case base rules" in {
    parseRules(baseCase) should have size 9
  }

  "findContainer" should "find the containers for the base case" in {
    findContainers("shiny gold", parseRules(baseCase)) shouldBe Set(
      "bright white",
      "muted yellow",
      "dark orange",
      "light red"
    )
  }

  it should "handle loops" in {
    val rules = Seq(
      "dark blue bags contain 1 bright white bag, 2 shiny gold bags, 1 dark blue bag."
    )
    findContainers("shiny gold", parseRules(rules)) shouldBe Set("dark blue")
  }

  "countBags" should "count the number of bags for the base case" in {
    countBags("shiny gold", parseRules(baseCase)) shouldBe 32
  }

  "Handy Haversacks" should "find how many bag colors can eventually contain at least one shiny gold bag" in {
    findContainers("shiny gold", parseRules(in)) should have size 103
  }

  it should "count how many individual bags are required inside your single shiny gold bag" in {
    countBags("shiny gold", parseRules(in)) shouldBe 1469
  }
}
