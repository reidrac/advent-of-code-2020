import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.io.Source

// https://adventofcode.com/2020/day/6
class DaySixSpec extends AnyFlatSpec with Matchers {
  val in = Source
    .fromResource("6.txt")
    .getLines()
    .toSeq
    .mkString("\n")
    .split("\\n\\n")
    .toSeq

  def count(groups: Seq[String]): Int =
    groups.map { answers =>
      answers.split("\n").flatten.toSet.size
    }.sum

  def countShared(groups: Seq[String]): Int =
    groups.map { answers =>
      val sets = answers.split("\n")
      sets.flatten
        .map { answer => (answer, sets.flatten.count(_ == answer)) }
        .filter { case (_, count) => count == sets.size }
        .toMap
        .keySet
        .size
    }.sum

  "count" should "count the number of 'yes' answers in a group" in {
    count(Seq("""|abcx
                 |abcy
                 |abcz""".stripMargin)) shouldBe 6
  }

  "countShared" should "count the number of shared 'yes' answers in all groups" in {
    countShared("""|abc
                   |
                   |a
                   |b
                   |c
                   |
                   |ab
                   |ac
                   |
                   |a
                   |a
                   |a
                   |a
                   |
                   |b""".stripMargin.split("\\n\\n").toSeq) shouldBe 6
  }

  "Custom Customs" should "sum the count of all 'yes' answers" in {
    count(in) shouldBe 6521
  }

  it should "sum the count of shared 'yes' answers" in {
    countShared(in) shouldBe 3305
  }
}
