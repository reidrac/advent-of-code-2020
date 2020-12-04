import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

// https://adventofcode.com/2020/day/4
class DayFourSpec extends AnyFlatSpec with Matchers {

  class PasswordValidator(resource: String) {
    val in = Source.fromResource(resource).getLines().toSeq.mkString("\n")

    val required = List(
      "byr",
      "iyr",
      "eyr",
      "hgt",
      "hcl",
      "ecl",
      "pid"
      // "cid" optional
    )

    val yearRe = "([0-9]{4})".r
    val heightRe = "([0-9]+)(cm|in)".r
    val colorRe = "#([0-9a-f]{6})".r
    val eyeColorRe = "(amb|blu|brn|gry|grn|hzl|oth)".r
    val pidRe = "([0-9]{9})".r

    private def toTokens(in: String): Map[String, String] =
      in.split("\\s")
        .map(_.split(":").toSeq)
        .flatMap {
          case Seq(key, value) => Some((key, value))
          case _               => None
        }
        .toMap

    def withValidKeys: Seq[Map[String, String]] =
      in.split("\\n\\n").toSeq.map(toTokens).filterNot { passport =>
        required.exists(!passport.keySet.contains(_))
      }

    def valid: Seq[Map[String, String]] =
      withValidKeys.filterNot(passport =>
        passport.toSeq
          .map {
            case ("byr", yearRe(y))         => y.toInt >= 1920 && y.toInt <= 2002
            case ("iyr", yearRe(y))         => y.toInt >= 2010 && y.toInt <= 2020
            case ("eyr", yearRe(y))         => y.toInt >= 2020 && y.toInt <= 2030
            case ("hgt", heightRe(h, "cm")) => h.toInt >= 150 && h.toInt <= 193
            case ("hgt", heightRe(h, "in")) => h.toInt >= 59 && h.toInt <= 76
            case ("hcl", colorRe(_))        => true
            case ("ecl", eyeColorRe(_))     => true
            case ("pid", pidRe(_))          => true
            case ("cid", _)                 => true
            case _                          => false
          }
          .exists(_ == false)
      )
  }

  "Passport Processing" should "count valid passports (keys)" in {
    new PasswordValidator("4.txt").withValidKeys.length shouldBe 228
  }

  it should "count valid passports" in {
    new PasswordValidator("4.txt").valid.length shouldBe 175
  }
}
