import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

// https://adventofcode.com/2020/day/5
class DayFiveSpec extends AnyFlatSpec with Matchers {
  val in = Source
    .fromResource("5.txt")
    .getLines()
    .map(_.strip)
    .toSeq

  def decode(code: String): Int = {
    def decoder(from: Int, to: Int, code: List[Char]): Int =
      code match {
        case ('F' | 'L') :: Nil  => from
        case ('B' | 'R') :: Nil  => to
        case ('F' | 'L') :: tail => decoder(from, from + (to - from) / 2, tail)
        case ('B' | 'R') :: tail =>
          decoder(from + 1 + (to - from) / 2, to, tail)
        case _ => throw new RuntimeException(s"Invalid code: $code")
      }

    val (row, column) = code.toList.splitAt(7)
    decoder(0, 7, column) + decoder(0, 127, row) * 8
  }

  "decode" should "decode base case" in {
    decode("FBFBBFFRLR") shouldBe 357
  }

  "Binary Boarding" should "find highest seat ID" in {
    in.map(decode).max shouldBe 965
  }

  it should "find my seat ID" in {
    in.map(decode)
      .sorted
      .sliding(2)
      .find {
        case a :: b :: Nil => b - a > 1
      }
      .map {
        case a :: b :: Nil => a + 1
      } shouldBe Some(524)
  }
}
