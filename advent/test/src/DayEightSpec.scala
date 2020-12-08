import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.io.Source

// https://adventofcode.com/2020/day/8
class DayEightSpec extends AnyFlatSpec with Matchers {

  val baseCase =
    """|nop +0
       |acc +1
       |jmp +4
       |acc +3
       |jmp -3
       |acc -99
       |acc +1
       |jmp -4
       |acc +6""".stripMargin
      .split("\n")
      .toSeq

  val in = Source
    .fromResource("8.txt")
    .getLines()
    .toSeq

  val instrRe = "([^\\s]+) ([+-]{1}[0-9]+)".r

  type Op = String
  type Arg = Int

  case class Instr(op: Op, arg: Arg)
  case class Program(
      pc: Int,
      acc: Int,
      code: Seq[Instr],
      trace: Seq[(Int, Int)]
  )

  val nop = Instr("nop", 0)

  object Program {
    def apply(in: Seq[String]): Program =
      Program(
        0,
        0,
        in.map { case instrRe(op, arg) => Instr(op, arg.toInt) },
        Nil
      )
  }

  implicit class ProgramOps(p: Program) {
    def patch(pc: Int, instr: Instr = nop): Program =
      p.copy(code = p.code.updated(pc, instr))

    def step: Program = {
      val (pc, acc) = p.code(p.pc) match {
        case Instr("nop", _) => (p.pc + 1, p.acc)
        case Instr("acc", v) => (p.pc + 1, p.acc + v)
        case Instr("jmp", d) => (p.pc + d, p.acc)
        case Instr(op, _) =>
          throw new RuntimeException(s"Unknown op $op at ${p.pc}")
      }
      p.copy(pc = pc, acc = acc, trace = p.trace.appended((pc, acc)))
    }

    def run: Either[Program, Program] = {
      @tailrec
      def _run(p: Program): Either[Program, Program] =
        p.step match {
          case n if n.pc >= n.code.size => Right(n)
          case n if p.trace.exists { case (pc, _) => pc == n.pc } => Left(p)
          case n => _run(n)
        }
      _run(p)
    }

    def runFix: Program =
      (0 to p.code.size - 1)
        .flatMap {
          case i if p.code(i).op == "jmp" => p.patch(i).run.toOption
          case _                          => None
        }
        .headOption
        .getOrElse(throw new RuntimeException("Fix not found"))
  }

  "run" should "tell the value in the accumulator when an instruction is run twice (base case)" in {
    val Left(p) = Program(baseCase).run
    p.acc shouldBe 5
  }

  "runFix" should "fix the program and tell the value of the accumulator when the program ends (base case)" in {
    Program(baseCase).runFix.acc shouldBe 8
  }

  "Handheld Halting" should "tell the value in the accumulator when an instruction is run twice" in {
    val Left(p) = Program(in).run
    p.acc shouldBe 1684
  }

  it should "fix the program and tell the value of the accumulator when the program ends" in {
    Program(in).runFix.acc shouldBe 2188
  }
}
