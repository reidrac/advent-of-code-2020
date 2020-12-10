import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

// https://adventofcode.com/2020/day/10
class DayTenSpec extends AnyFlatSpec with Matchers {

  val baseCase =
    """|16
       |10
       |15
       |5
       |1
       |11
       |7
       |19
       |6
       |12
       |4""".stripMargin.split("\n").toSeq.map(_.toInt)

  val in = Source
    .fromResource("10.txt")
    .getLines()
    .toSeq
    .map(_.toInt)

  def countJolts(in: Seq[Int]): Int = {
    val (a, b, _) =
      in.prepended(0)
        .appended(in.max + 3)
        .sorted
        .foldLeft((0, 0, 0)) {
          case ((a, b, prev), jolts) if jolts - prev == 1 => (a + 1, b, jolts)
          case ((a, b, prev), jolts) if jolts - prev == 3 => (a, b + 1, jolts)
          case ((a, b, _), jolts)                         => (a, b, jolts)
        }
    a * b
  }

  "countJolts" should "the number of 1-jolt differences multiplied by the number of 3-jolt differences (case base)" in {
    countJolts(baseCase) shouldBe 35
  }

  "Adapter Array" should "the number of 1-jolt differences multiplied by the number of 3-jolt differences" in {
    countJolts(in) shouldBe 2048
  }
}
