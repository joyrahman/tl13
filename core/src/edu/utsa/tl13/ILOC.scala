package edu.utsa.tl13

import Parse._

object ILOC {

  case class Block(label: Int, instrs: Seq[Instruction])

  case class IlocifyState(idents: Map[String,Int], register: Int, currBlock: Block, blocks: Set[Block]) {
    def appendInstruction(instr: Instruction) =
      copy(currBlock = currBlock.copy(instrs = currBlock.instrs :+ instr))
    def incRegister = copy(register = register+1)
    def nextBlockLabel = (this.blocks + this.currBlock).map(_.label).max + 1
    def newBlock(label: Int) = copy(blocks = blocks + currBlock, currBlock = Block(label, Vector()))
  }

  abstract class Instruction

  case class loadI(c1: Int, r2: Int) extends Instruction

  case class add(r1: Int, r2: Int, r3: Int) extends Instruction

  case class sub(r1: Int, r2: Int, r3: Int) extends Instruction

  case class mult(r1: Int, r2: Int, r3: Int) extends Instruction

  case class div(r1: Int, r2: Int, r3: Int) extends Instruction

  case class mod(r1: Int, r2: Int, r3: Int) extends Instruction

  case class cmp_EQ(r1: Int, r2: Int, r3: Int) extends Instruction

  case class cmp_NE(r1: Int, r2: Int, r3: Int) extends Instruction

  case class cmp_LE(r1: Int, r2: Int, r3: Int) extends Instruction

  case class cmp_GE(r1: Int, r2: Int, r3: Int) extends Instruction

  case class cmp_LT(r1: Int, r2: Int, r3: Int) extends Instruction

  case class cmp_GT(r1: Int, r2: Int, r3: Int) extends Instruction

  case class writeInt(r1: Int) extends Instruction

  case class readInt(r1: Int) extends Instruction

  case class i2i(r1: Int, r2: Int) extends Instruction

  case class cbr(r1: Int, l2: Int, l3: Int) extends Instruction

  case class jumpI(l1: Int) extends Instruction

  case class exit() extends Instruction

  def ilocifyNum(num: Num, state: IlocifyState): Pair[Int,IlocifyState] =
    (state.register, state.appendInstruction(loadI(num.value.toInt, state.register)).incRegister)

  def ilocifyBoolLit(bool: BoolLit, state: IlocifyState): Pair[Int,IlocifyState] = {
    val v = if (bool.value == "true") 1 else 0
    (state.register, state.appendInstruction(loadI(v, state.register)).incRegister)
  }

  def ilocifyIdent(ident: Ident, state: IlocifyState): Pair[Int,IlocifyState] =
    (state.idents(ident.value), state)

  def ilocifyOp(op: Op, state: IlocifyState): Pair[Int,IlocifyState] = {
    def aux(e1: Expr, e2: Expr)(f: (Int,Int,Int) => Instruction): (Int,IlocifyState) = {
      val (r1, s1) = ilocifyExpr(e1, state)
      val (r2, s2) = ilocifyExpr(e2, s1)
      (s2.register, s2.appendInstruction(f(r1, r2, s2.register)).incRegister)
    }
    val (o, f) = op match { case Op(o, e1, e2) => (o, aux(e1, e2)_) }
    o match {
      case "+"   => f(add)
      case "-"   => f(sub)
      case "*"   => f(mult)
      case "div" => f(div)
      case "mod" => f(mod)
      case "="   => f(cmp_EQ)
      case "!="  => f(cmp_NE)
      case "<="  => f(cmp_LE)
      case ">="  => f(cmp_GE)
      case "<"   => f(cmp_LT)
      case ">"   => f(cmp_GT)
    }
  }

  def ilocifyExpr(expr: Expr, state: IlocifyState): Pair[Int, IlocifyState] =
    expr match {
      case e: Num     => ilocifyNum(e, state)
      case e: BoolLit => ilocifyBoolLit(e, state)
      case e: Ident   => ilocifyIdent(e, state)
      case e: Op      => ilocifyOp(e, state)
    }

  def ilocifyStatementSeq(stmts: StatementSeq, state: IlocifyState): IlocifyState =
    stmts.stmts.foldLeft(state) { (state, stmt) => ilocifyStatement(stmt, state) }

  def ilocifyStatement(stmt: Statement, state: IlocifyState): IlocifyState = stmt match {
      case a: Assignment => ilocifyAssignment(a, state)
      case w: WriteInt   => ilocifyWriteInt(w, state)
      case i: If         => ilocifyIf(i, state)
      case w: While      => ilocifyWhile(w, state)
    }

  def ilocifyAssignment(asgn: Assignment, state: IlocifyState): IlocifyState = asgn match {
      case a: AsgnExpr => ilocifyAsgnExpr(a, state)
      case r: ReadInt  => ilocifyReadInt(r, state)
    }

  def ilocifyAsgnExpr(asgn: AsgnExpr, state: IlocifyState): IlocifyState = {
    val (r, newState) = ilocifyExpr(asgn.expr, state)
    val i = i2i(r, newState.idents(asgn.ident.value))
    newState.appendInstruction(i)
  }

  def ilocifyReadInt(readIntNode: ReadInt, state: IlocifyState): IlocifyState = {
    val r = state.idents(readIntNode.ident.value)
    val ri = readInt(r)
    state.appendInstruction(ri)
  }

  def ilocifyWriteInt(writeIntNode: WriteInt, state: IlocifyState): IlocifyState = {
    val (register, newState) = ilocifyExpr(writeIntNode.expr, state)
    val wi = writeInt(register)
    newState.appendInstruction(wi)
  }

  def ilocifyIf(ifStmt: If, state: IlocifyState): IlocifyState = {
    val thenLabel = state.nextBlockLabel
    val elseLabel = if (ifStmt.els.isEmpty) None else Some(thenLabel + 1)
    val afterLabel = if (ifStmt.els.isEmpty) thenLabel + 1 else thenLabel + 2

    val (r1, newState0) = ilocifyExpr(ifStmt.expr, state)
    val l2 = thenLabel
    val l3 = if (elseLabel.isEmpty) afterLabel else elseLabel.get
    val newState1 = newState0.appendInstruction(cbr(r1, l2, l3)).newBlock(l2)
    val newLabel = if (elseLabel.isEmpty) afterLabel else elseLabel.get
    val newState2 = ilocifyStatementSeq(ifStmt.thn, newState1)
      .appendInstruction(jumpI(afterLabel))
      .newBlock(newLabel)

    if (ifStmt.els.isEmpty)
      newState2
    else ilocifyStatementSeq(ifStmt.els.get, newState2)
      .appendInstruction(jumpI(afterLabel))
      .newBlock(afterLabel)
  }

  def ilocifyWhile(whileStmt: While, state: IlocifyState): IlocifyState = {
    val exprLabel = state.nextBlockLabel
    val ssLabel = exprLabel + 1
    val afterLabel = exprLabel + 2

    val newState0 = state.appendInstruction(jumpI(exprLabel)).newBlock(exprLabel)
    val (r, newState1) = ilocifyExpr(whileStmt.expr, newState0)
    val newState2 = newState1.appendInstruction(cbr(r, ssLabel, afterLabel)).newBlock(ssLabel)

    ilocifyStatementSeq(whileStmt.stmts, newState2)
      .appendInstruction(jumpI(exprLabel))
      .newBlock(afterLabel)
  }

  def ilocifyProgram(program: Program): Iterable[Block] = {
    val stmts = program.stmts
    val idents = program.decls.decls.map(_.value).zip(Stream.from(0)).toMap
    val reg = idents.map(_._2).max + 1
    val block = Block(0, program.decls.decls.map(d => loadI(0, idents(d.value))))
    val blocks = Set[Block]()
    val newState = ilocifyStatementSeq(stmts, IlocifyState(idents, reg, block, blocks)).appendInstruction(exit())
    newState.blocks + newState.currBlock
  }

  def firstBlock(blocks: Iterable[Block]): Block =
    (blocks.toSet &~ blocks.map(block => childBlocks(block, blocks)).flatten.toSet).head

  def childBlocks[A](block: Block, all: Iterable[Block]): Iterable[Block] =
    if (block.instrs.isEmpty) Vector()
    else block.instrs.last match {
      case cbr(_, l2, l3) => Vector(l2, l3).map(x => all.find(y => x == y.label)).filter(!_.isEmpty).map(_.get)
      case jumpI(l1)      => Vector(all.find(x => x.label == l1)).filter(!_.isEmpty).map(_.get)
      case _              => Vector()
    }

  def mkBlockLinkMap(blocks: Iterable[Block]): Map[Block, Iterable[Block]] =
    blocks.foldLeft(Map[Block, Iterable[Block]]())((acc,block) => acc + (block -> childBlocks(block, blocks)))

  // def prewalkBlocks[A](f: (A, Block) => A, acc: A, blocks: Iterable[Block]): A = {
  //   val linkMap = mkBlockLinkMap(blocks: Iterable[Block])
  //   def aux(acc: A, block: Block, visited: Iterable[Block]): A = {
  //     if (visited.exists(_ == block)) {
  //       acc
  //     } else {
  //       val res = f(acc, block)
  //       linkMap(block).foldLeft(res)((a,b) => aux(a, b, visited.toSeq :+ block))
  //     }
  //   }
  //   aux(acc, firstBlock(blocks), Vector())
  // }

  /* @TODO turn this into unit tests
   import edu.utsa.tl13.Parse._
   import edu.utsa.tl13.ILOC._

   var state = IlocifyState(Map(), 0, Block(0, Vector()), Set())
   print( ilocifyNum(Num("1"), state) )
   print( ilocifyBoolLit(BoolLit("true"), state) )
   print( ilocifyBoolLit(BoolLit("false"), state) )
   print( ilocifyIdent(Ident("X"), state.copy(idents = Map("X" -> 0, "Y" -> 1), register = 2)) )
   print( ilocifyIdent(Ident("Y"), state.copy(idents = Map("X" -> 0, "Y" -> 1), register = 2)) )

   print( ilocifyOp(Op("+", Num("1"), Num("2")), state) )

   print( ilocifyOp(Op("+", Ident("X"), Num("2")), state.copy(idents = Map("X" -> 0), register = 1)) )
   print( ilocifyOp(Op("+", Num("2"), Ident("X")), state.copy(idents = Map("X" -> 0), register = 1)) )

   val e = Op("+", Num("3"), Num("4"))
   print( ilocifyExpr(e, state) )

   val e2 = Op("+", Num("1"), Ident("X"))
   print( ilocifyExpr(e2, state.copy(idents = Map("X" -> 0), register = 1)) )

   val e1 = Op("+", Op("+", Num("5"), Op("*", Ident("X"), Num("7"))), Ident("Y"))
   print( ilocifyExpr(e1, state.copy(idents = Map("X" -> 0, "Y" -> 1), register = 2)) )

   val e3 = Op("*", Op("+", Num("5"), Op("+", Num("2"), Ident("x"))), Num("3"))
   print( ilocifyExpr(e3, state.copy(idents = Map("x" -> 7), register = 46)) )

   print( ilocifyWriteInt(WriteInt(Op("+", Num("1"), Num("2"))), state) )

   print( ilocifyReadInt(ReadInt(Ident("X")), state.copy(idents = Map("X" -> 0), register = 1)) )
   print( ilocifyReadInt(ReadInt(Ident("Y")), state.copy(idents = Map("X" -> 0, "Y" -> 1), register = 2)) )

   print( ilocifyAsgnExpr(AsgnExpr(Ident("X"), Op("+", Num("1"), Num("2"))),
                          state.copy(idents = Map("X" -> 0), register = 1)) )
   print( ilocifyAsgnExpr(AsgnExpr(Ident("Y"), Op("+", Num("1"), Num("2"))),
                          state.copy(idents = Map("X" -> 0, "Y" -> 1), register = 2)) )

   print( ilocifyIf(If(Op("=", Num("1"), Num("2")), StatementSeq(), None), state) )
   print( ilocifyIf(If(Op("=", Num("1"), Num("2")), StatementSeq(), Some(StatementSeq())), state) )

   print( ilocifyWhile(While(Op("!=", Num("1"), Num("2")), StatementSeq()), state) )
   print( ilocifyWhile(While(Op("!=", Num("1"), Num("2")), StatementSeq(WriteInt(Num("3")))), state) )

   var decls = Decls(Decl("X", TL13Int()), Decl("Y", TL13Int()))
   print( ilocifyProgram(Program(decls, StatementSeq())) )
   print( ilocifyProgram(Program(decls, StatementSeq(ReadInt(Ident("X"))))) )
   print( ilocifyProgram(Program(decls, StatementSeq(If(Op("!=", Num("1"), Ident("X")), StatementSeq(), None)))) )

   print( firstBlock(ilocifyProgram(Program(decls, StatementSeq(If(Op("!=", Num("1"), Ident("X")), StatementSeq(), None))))) )

   print( mkBlockLinkMap(ilocifyProgram(Program(decls, StatementSeq(If(Op("!=", Num("1"), Ident("X")), StatementSeq(), None))))) )

   var program = ilocifyProgram(Program(decls, StatementSeq(If(Op("!=", Num("1"), Ident("X")), StatementSeq(), None))))
   print( prewalkBlocks((a:Int,b) => a + b.instrs.length, 0, program) )



   */

}
