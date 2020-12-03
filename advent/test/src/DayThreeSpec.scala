import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.io.Source

// https://adventofcode.com/2020/day/3
class DayThreeSpec extends AnyFlatSpec with Matchers {
  val in =
    Source
      .fromResource("3.txt")
      .getLines()
      .toSeq

  case class Mountain(width: Int, height: Int, data: Seq[Char]) {
    def get(x: Int, y: Int): Option[Char] = {
      if (y < height) Some(data(x % width + y * width))
      else None
    }

    // traverses the mountaing using slope (right, down), returning the number of trees
    def countTrees(right: Int, down: Int): Int = {
      @tailrec
      def move(x: Int, y: Int, start: Int): Int =
        get(x, y) match {
          case Some('#') => move(x + right, y + down, start + 1)
          case Some('.') => move(x + right, y + down, start)
          case _         => start
        }
      move(0, 0, 0)
    }
  }

  "Mountain.get" should "wrap around width" in {
    val m = Mountain(in(0).length, in.length, in.toList.flatten)

    m.get(0, 0) shouldBe Some('.')
    m.get(in(0).length, 0) shouldBe m.get(0, 0)
    m.get(0, in.length) shouldBe None
  }

  "Toboggan Trajectory" should "count trees in trajectory with slope right 3, down 1 (base case)" in {
    val in =
      Source
        .fromResource("3-base.txt")
        .getLines()
        .toSeq

    Mountain(in(0).length, in.length, in.toList.flatten)
      .countTrees(3, 1) shouldBe 7
  }

  it should "count trees in trajectory with slope right 3, down 1" in {
    val m = Mountain(in(0).length, in.length, in.toList.flatten)
      .countTrees(3, 1) shouldBe 247
  }

  it should "count trees in trajectory with multiple slopes" in {
    val m = Mountain(in(0).length, in.length, in.toList.flatten)

    List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)).foldLeft[Long](1) {
      case (acc, (right, down)) => acc * m.countTrees(right, down)
    } shouldBe 2983070376L
  }
}
