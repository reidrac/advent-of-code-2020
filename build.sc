import mill._, scalalib._, scalafmt._

trait AdventModule extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.13.4"
}

object one extends AdventModule
object two extends AdventModule
