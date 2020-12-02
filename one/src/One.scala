import scala.io.Source

object One {

  def main(args: Array[String]): Unit = {

    val in =
      Source
        .fromResource("1.txt")
        .getLines()
        .map(_.toInt)
        .toSeq

    (for {
      a <- in
      b <- in
      if a != b && a + b == 2020
    } yield a * b).headOption.map(println)
    // 866436

    (for {
      a <- in
      b <- in
      c <- in
      if a != b && a != c && b != c && a + b + c == 2020
    } yield a * b * c).headOption.map(println)
    // 276650720
  }
}
