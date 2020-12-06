import mill._, scalalib._, scalafmt._

object advent extends ScalaModule {
  def scalaVersion = "2.13.4"

  object test extends Tests with ScalafmtModule {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.2")
    def testFrameworks = Seq("org.scalatest.tools.Framework")

    def testOnly(args: String*) = T.command {
      super.runMain("org.scalatest.run", args: _*)
    }
  }
}
