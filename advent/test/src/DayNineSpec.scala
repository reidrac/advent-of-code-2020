import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.io.Source

// https://adventofcode.com/2020/day/9
class DayNineSpec extends AnyFlatSpec with Matchers {

  val baseCase =
    """|35
       |20
       |15
       |25
       |47
       |40
       |62
       |55
       |65
       |95
       |102
       |117
       |150
       |182
       |127
       |219
       |299
       |277
       |309
       |576""".stripMargin.split("\n").toSeq.map(_.toLong)

  val in = Source
    .fromResource("9.txt")
    .getLines()
    .toSeq
    .map(_.toLong)

  def findException(in: Seq[Long], preamble: Int): Option[Long] =
    in.zipWithIndex.drop(preamble).collectFirst {
      case (n, i)
          if !in
            .slice(i - preamble, i)
            .exists(v =>
              in.slice(i - preamble, i).filter(_ != v).exists(_ + v == n)
            ) =>
        n
    }

  def findSeqMatching(in: Seq[Long], value: Long): Option[Seq[Long]] = {
    @tailrec
    def _find(n: Int): Option[Seq[Long]] = {
      in.sliding(n).find(_.sum == value) match {
        case None if n + 1 == in.size => None
        case None                     => _find(n + 1)
        case seq                      => seq
      }
    }

    _find(2)
  }

  "findException" should "find the first number that is not the sum of 2 of the prev 5 numbers, after preamble of 5 (case base)" in {
    findException(baseCase, 5) shouldBe Some(127L)
  }

  "findSeqMatching" should "find the encryption weakness (case base)" in {
    val Some(ex) = findException(baseCase, 5)
    val Some(seq) = findSeqMatching(baseCase, ex)

    seq shouldBe Seq(15, 25, 47, 40)
    seq.min + seq.max shouldBe 62
  }

  "Encoding Error" should "find the first number that is not the sum of 2 of the prev 25 numbers, after preamble of 25" in {
    findException(in, 25) shouldBe Some(57195069L)
  }

  it should "find the encryption weakness" in {
    val Some(ex) = findException(in, 25)
    val Some(seq) = findSeqMatching(in, ex)

    seq.min + seq.max shouldBe 7409241
  }
}
