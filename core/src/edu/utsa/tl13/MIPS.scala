package edu.utsa.tl13

import ILOC._

/** MIPS handling */
object MIPS {

  /** Represents a MIPS block */
  case class Block(label: Int, instrs: Instruction*) {
    override def toString = "B%d:\n%s\n".format(label, instrs.map(i => "\t" + i.toString + "\n").mkString)
  }

  /** Represents a MIPS program */
  case class Program(blocks: Block*) {
    override def toString = "\t.data\nnewline:\t.asciiz \"\\n\"\n\t.text\n\t.globl main\nmain:\n\tli $fp, 0x7ffffffc\n\n%s"
      .format(blocks.map(b => b.toString).mkString)
  }

  /** Represents a MIPS instruction */
  abstract class Instruction

  /** MIPS load immediate instruction */
  case class li(r1: String, c2: Int) extends Instruction {
    override def toString = "li %s, %d".format(r1, c2)
  }

  /** MIPS store word instruction */
  case class sw(r1: String, r2: Int) extends Instruction {
    override def toString = "sw %s, %d($fp)".format(r1, r2)
  }

  /** MIPS load word instruction */
  case class lw(r1: String, r2: Int) extends Instruction {
    override def toString = "lw %s, %d($fp)".format(r1, r2)
  }

  /** MIPS add unsigned instruction */
  case class addu(r1: String, r2: String, r3: String) extends Instruction {
    override def toString = "addu %s, %s, %s".format(r1, r2, r3)
  }

  /** MIPS subtract unsigned instruction */
  case class subu(r1: String, r2: String, r3: String) extends Instruction {
    override def toString = "subu %s, %s, %s".format(r1, r2, r3)
  }

  /** MIPS multiply instruction */
  case class mul(r1: String, r2: String, r3: String) extends Instruction {
    override def toString = "mul %s, %s, %s".format(r1, r2, r3)
  }

  /** MIPS div instruction */
  case class div(r1: String, r2: String, r3: String) extends Instruction {
    override def toString = "div %s, %s, %s".format(r1, r2, r3)
  }

  /** MIPS move from hi instruction */
  case class mfhi(r1: String) extends Instruction {
    override def toString = "mfhi %s".format(r1)
  }

  /** MIPS set equal instruction */
  case class seq(r1: String, r2: String, r3: String) extends Instruction {
    override def toString = "seq %s, %s, %s".format(r1, r2, r3)
  }

  /** MIPS set not equal instruction */
  case class sne(r1: String, r2: String, r3: String) extends Instruction {
    override def toString = "sne %s, %s, %s".format(r1, r2, r3)
  }

  /** MIPS set less than equal instruction */
  case class sle(r1: String, r2: String, r3: String) extends Instruction {
    override def toString = "sle %s, %s, %s".format(r1, r2, r3)
  }

  /** MIPS set less than instruction */
  case class slt(r1: String, r2: String, r3: String) extends Instruction {
    override def toString = "slt %s, %s, %s".format(r1, r2, r3)
  }

  /** MIPS set greater than equal instruction */
  case class sge(r1: String, r2: String, r3: String) extends Instruction {
    override def toString = "sge %s, %s, %s".format(r1, r2, r3)
  }

  /** MIPS set greater than instruction */
  case class sgt(r1: String, r2: String, r3: String) extends Instruction {
    override def toString = "sgt %s, %s, %s".format(r1, r2, r3)
  }

  /** MIPS add instruction instruction */
  case class add(r1: String, r2: String, r3: String) extends Instruction {
    override def toString = "add %s, %s, %s".format(r1, r2, r3)
  }

  /** MIPS system call instruction */
  case class syscall() extends Instruction {
    override def toString = "syscall"
  }

  /** MIPS load address instruction */
  case class la(r1: String, r2: String) extends Instruction {
    override def toString = "la %s, %s".format(r1, r2)
  }

  /** MIPS branch on not equal instruction */
  case class bne(r1: String, r2: String, b3: Int) extends Instruction {
    override def toString = "bne %s, %s, B%d".format(r1, r2, b3)
  }

  /** MIPS jump instruction */
  case class j(b1: Int) extends Instruction {
    override def toString = "j B%d".format(b1)
  }

  /** Calculates the stack location of a virtual register */
  private def vregToFp(r: Int): Int = 0 - 4 * r

  /** Convert ILOC blocks to a MIPS program */
  def mipsifyProgram(blocks: Seq[ILOC.Block]): Program =
    Program(mipsifyBlocks(blocks):_*)

  /** Convert ILOC blocks to MIPS blocks */
  def mipsifyBlocks(blocks: Seq[ILOC.Block]): Seq[Block] =
    blocks.map(b => Block(b.label, b.instrs.flatMap(i => mipsifyInstruction(i)):_*))

  /** Convert an ILOC instruction to equivalent MIPS instruction(s) */
  def mipsifyInstruction(instr: ILOC.Instruction): Seq[Instruction] = instr match {
      case ILOC.loadI(c1, r2) => Vector(li("$t0", c1), sw("$t0", vregToFp(r2)))
      case ILOC.add(r1, r2, r3) =>
        Vector(lw("$t0", vregToFp(r1)), lw("$t1", vregToFp(r2)), addu("$t0", "$t0", "$t1"), sw("$t0", vregToFp(r3)))
      case ILOC.sub(r1, r2, r3) =>
        Vector(lw("$t0", vregToFp(r1)), lw("$t1", vregToFp(r2)), subu("$t0", "$t0", "$t1"), sw("$t0", vregToFp(r3)))
      case ILOC.mult(r1, r2, r3) =>
        Vector(lw("$t0", vregToFp(r1)), lw("$t1", vregToFp(r2)), mul("$t0", "$t0", "$t1"), sw("$t0", vregToFp(r3)))
      case ILOC.div(r1, r2, r3) =>
        Vector(lw("$t0", vregToFp(r1)), lw("$t1", vregToFp(r2)), div("$t0", "$t0", "$t1"), sw("$t0", vregToFp(r3)))
      case ILOC.mod(r1, r2, r3) =>
        Vector(lw("$t0", vregToFp(r1)), lw("$t1", vregToFp(r2)), div("$t0", "$t0", "$t1"),
               mfhi("$t0"), sw("$t0", vregToFp(r3)))
      case ILOC.cmp_EQ(r1, r2, r3) =>
        Vector(lw("$t0", vregToFp(r1)), lw("$t1", vregToFp(r2)), seq("$t0", "$t0", "$t1"), sw("$t0", vregToFp(r3)))
      case ILOC.cmp_NE(r1, r2, r3) =>
        Vector(lw("$t0", vregToFp(r1)), lw("$t1", vregToFp(r2)), sne("$t0", "$t0", "$t1"), sw("$t0", vregToFp(r3)))
      case ILOC.cmp_LT(r1, r2, r3) =>
        Vector(lw("$t0", vregToFp(r1)), lw("$t1", vregToFp(r2)), slt("$t0", "$t0", "$t1"), sw("$t0", vregToFp(r3)))
      case ILOC.cmp_LE(r1, r2, r3) =>
        Vector(lw("$t0", vregToFp(r1)), lw("$t1", vregToFp(r2)), sle("$t0", "$t0", "$t1"), sw("$t0", vregToFp(r3)))
      case ILOC.cmp_GT(r1, r2, r3) =>
        Vector(lw("$t0", vregToFp(r1)), lw("$t1", vregToFp(r2)), sgt("$t0", "$t0", "$t1"), sw("$t0", vregToFp(r3)))
      case ILOC.cmp_GE(r1, r2, r3) =>
        Vector(lw("$t0", vregToFp(r1)), lw("$t1", vregToFp(r2)), sge("$t0", "$t0", "$t1"), sw("$t0", vregToFp(r3)))
      case ILOC.writeInt(r1) =>
        Vector(li("$v0", 1), lw("$t0", vregToFp(r1)), add("$a0", "$t0", "$zero"), syscall(),
               li("$v0", 4), la("$a0", "newline"), syscall())
      case ILOC.readInt(r1) =>
        Vector(li("$v0", 5), syscall(), add("$t0", "$v0", "$zero"), sw("$t0", vregToFp(r1)))
      case ILOC.i2i(r1, r2) =>
        Vector(lw("$t0", vregToFp(r1)), add("$t0", "$t0", "$zero"), sw("$t0", vregToFp(r2)))
      case ILOC.cbr(r1, b2, b3) => Vector(lw("$t0", vregToFp(r1)), bne("$t0", "$zero", b2), j(b3))
      case ILOC.jumpI(b1) => Vector(j(b1))
      case ILOC.exit() => Vector(li("$v0", 10), syscall())
    }

}
