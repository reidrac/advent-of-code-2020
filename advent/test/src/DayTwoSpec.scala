import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class DayTwoSpec extends AnyFlatSpec {

  val in =
    Source
      .fromResource("2.txt")
      .getLines()
      .toSeq

  val pattern = "^([\\d]+)-([\\d]+) ([a-z]): ([a-z]+)$".r

  def extract(line: String): (Int, Int, Char, String) =
    line match {
      case pattern(from, to, letter, password) =>
        (from.toInt, to.toInt, letter.charAt(0), password)
    }

  "Password Philosophy" should "check how many paswords match corporate policy" in {
    assert(
      in.map(extract)
        .map {
          case (from, to, letter, password) =>
            (from, to, password.count(_ == letter))
        }
        .filter {
          case (from, to, count) =>
            count >= from && count <= to
        }
        .length == 556
    )
  }

  it should "check how many paswords match corporate policy (position)" in {
    assert(
      in.map(extract)
        .filter {
          case (pos1, pos2, _, password) =>
            pos1 <= password.length && pos2 <= password.length
        }
        .filter {
          case (pos1, pos2, letter, password) =>
            (password.charAt(pos1 - 1) == letter && password
              .charAt(pos2 - 1) != letter) || (password
              .charAt(pos1 - 1) != letter && password
              .charAt(pos2 - 1) == letter)
        }
        .length == 605
    )
  }
}
