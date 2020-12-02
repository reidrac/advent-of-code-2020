import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

// https://adventofcode.com/2020/day/1
class DayOneSpec extends AnyFlatSpec with Matchers {
  val in =
    Source
      .fromResource("1.txt")
      .getLines()
      .map(_.toInt)
      .toSeq

  "Report Repair" should "find the two entries that sum to 2020" in {
    (for {
      a <- in
      b <- in
      if a != b && a + b == 2020
    } yield a * b).headOption shouldBe Some(866436)
  }

  it should "find the three entries that sum to 2020" in {
    (for {
      a <- in
      b <- in
      c <- in
      if a != b && a != c && b != c && a + b + c == 2020
    } yield a * b * c).headOption shouldBe Some(276650720)
  }
}
