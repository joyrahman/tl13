package edu.utsa.tl13

import edu.utsa.tl13
import Parser._
import Scanner._
import UnitTest._

/** [[Parser]] unit tests */
object ParserTests {

  /** Dummy [[Parser.Node]] for testing */
  case class Dummy(i: Int, x: Option[Dummy], y: Option[Dummy]) extends Node {
    def children = List(x, y).foldLeft(Vector[Dummy]()) {
        (acc,node) => if (!node.isEmpty) acc :+ node.get else acc
      }
    def value = "dummy"
  }

  /** Dummy tree for testing */
  val dummyTree = Dummy(1,
                        Some(Dummy(2,
                                   Some(Dummy(3, None, None)),
                                   Some(Dummy(4, None, None)))),
                        Some(Dummy(5, None, None)))



  /** Tests [[Parser.Node.prewalk]] */
  val prewalkTests =
    Test("prewalk", () => assertEqual(dummyTree.prewalk(Vector[Int]())
                                        { (acc,node) => acc :+ (node.asInstanceOf[Dummy].i) },
                                      Vector(1,2,3,4,5)))

  /** [[Parser.parseProgram]] unit tests */
  val parseProgramTests =
    TestGroup("parseProgram",
              TestGroup("good",
                        mkParseProgramSuccess("program begin end",
                                              Program(Decls(), StatementSeq())),
                        mkParseProgramSuccess("program var A as int ; begin writeInt 1 ; end",
                                              Program(Decls(Decl("A", Type("int"))),
                                                      StatementSeq(WriteInt(Num("1")))))
              ),
              TestGroup("bad",
                        mkParseProgramFailure[Program,BadMatchError]("1 begin end"),
                        mkParseProgramFailure[Program,BadMatchError]("program 1 end"),
                        mkParseProgramFailure[Program,BadMatchError]("program begin 1"),
                        mkParseProgramFailure[Program,BadMatchError]("program begin end 1"),
                        mkParseProgramFailure[Program,EOFError]("program begin"),
                        mkParseProgramFailure[Program,EOFError]("program"),
                        mkParseProgramFailure[Program,BadMatchError]("")
              )
    )

  /** [[Parser.parseDeclarations]] unit tests */
  val parseDeclsTests =
    TestGroup("parseDeclarations",
              TestGroup("good",
                        mkParseDeclsSuccess("", Decls(), List(Token("",0,0))),
                        mkParseDeclsSuccess("1", Decls(), List(Token("1",0,0))),
                        mkParseDeclsSuccess("var A as int ;", Decls(Decl("A", Type("int")))),
                        mkParseDeclsSuccess("var A as int ; var B as bool ;",
                                            Decls(Decl("A", Type("int")), Decl("B", Type("bool")))),
                        mkParseDeclsSuccess("var A as int ; begin",
                                            Decls(Decl("A", Type("int"))), List(Token("begin",0,0)))
              )
    )

  /** [[Parser.parseStatementSeq]] unit tests */
  val parseStmtSeqTests =
    TestGroup("parseStatementSeq",
              TestGroup("good",
                        mkParseStmtSeqSuccess("", StatementSeq(), List(Token("",0,0))),
                        mkParseStmtSeqSuccess("writeInt 1 ;", StatementSeq(WriteInt(Num("1")))),
                        mkParseStmtSeqSuccess("writeInt 1 ; writeInt 2 ;",
                                              StatementSeq(WriteInt(Num("1")), WriteInt(Num("2")))),
                        mkParseStmtSeqSuccess("writeInt 1 ; while 1 + 2 do end ;",
                                              StatementSeq(WriteInt(Num("1")),
                                                           While(Op("+", Num("1"), Num("2")), StatementSeq()))),
                        mkParseStmtSeqSuccess("writeInt 1 ; end",
                                              StatementSeq(WriteInt(Num("1"))), List(Token("end",0,0))),
                        mkParseStmtSeqSuccess("writeInt 1 ; writeInt 2 ; end",
                                              StatementSeq(WriteInt(Num("1")), WriteInt(Num("2"))),
                                              List(Token("end",0,0)))
              )
    )

  /** [[Parser.parseStatement]] unit tests */
  val parseStmtTests =
    TestGroup("parseStatement",
              TestGroup("good",
                        mkParseStmtSuccess("A := 1 + 2",
                                           AsgnExpr(Ident("A"), Op("+", Num("1"), Num("2")))),
                        mkParseStmtSuccess("if 1 + 2 then end",
                                           If(Op("+", Num("1"), Num("2")), StatementSeq(), None)),
                        mkParseStmtSuccess("while 1 + 2 do end",
                                           While(Op("+", Num("1"), Num("2")), StatementSeq())),
                        mkParseStmtSuccess("writeInt 3", WriteInt(Num("3")))
              ),
              TestGroup("bad",
                        mkParseStmtFailure[Statement,BadMatchError]("*")
              )
    )

  /** [[Parser.parseAssignment]] unit tests */
  val parseAsgnTests =
    TestGroup("parseAssignment",
              TestGroup("good",
                        mkParseAsgnSuccess("A := readInt", ReadInt(Ident("A"))),
                        mkParseAsgnSuccess("A := 1 + 2", AsgnExpr(Ident("A"), Op("+", Num("1"), Num("2"))))
              ),
              TestGroup("bad",
                        mkParseAsgnFailure[Assignment,BadMatchError]("A readInt"),
                        mkParseAsgnFailure[Assignment,BadMatchError]("A 1 + 2"),
                        mkParseAsgnFailure[Assignment,EOFError]("A :="),
                        mkParseAsgnFailure[Assignment,EOFError]("A")
              )
    )

  /** [[Parser.parseIfStatement]] unit tests */
  val parseIfTests =
    TestGroup("parseIfStatement",
              TestGroup("good",
                        mkParseIfSuccess("if 1 + 2 then writeInt 3 ; end",
                                         If(Op("+", Num("1"), Num("2")),
                                            StatementSeq(WriteInt(Num("3"))), None)),
                        mkParseIfSuccess("if 1 + 2 then writeInt 3 ; else writeInt 4 ; end",
                                         If(Op("+", Num("1"), Num("2")),
                                            StatementSeq(WriteInt(Num("3"))),
                                            Some(StatementSeq(WriteInt(Num("4"))))))
              ),
              TestGroup("bad",
                        mkParseIfFailure[If,BadMatchError]("if then writeInt 3 ; end"),
                        mkParseIfFailure[If,BadMatchError]("if 1 + 2 then writeInt 3 ; 3 end"),
                        mkParseIfFailure[If,BadMatchError]("if 1 + 2 + then writeInt 3 ; end"),
                        mkParseIfFailure[If,BadMatchError]("if 1 + 2 then writeInt 3 ; else writeInt 4 ;"),
                        mkParseIfFailure[If,BadMatchError]("if 1 + 2 then writeInt 3 ; else"),
                        mkParseIfFailure[If,EOFError]("if 1 + 2 then writeInt 3 ;"),
                        mkParseIfFailure[If,EOFError]("if 1 + 2 then"),
                        mkParseIfFailure[If,EOFError]("if 1 + 2"),
                        mkParseIfFailure[If,EOFError]("if")
              )
    )

  /** [[Parser.parseElseClause]] unit tests */
  val parseElseTests =
    TestGroup("parseElseClause",
              TestGroup("good",
                        mkParseElseSuccess("else writeInt 3 ;", StatementSeq(WriteInt(Num("3")))),
                        mkParseElseSuccess("else", StatementSeq())))

  /** [[Parser.parseWhileStatement]] unit tests */
  val parseWhileTests =
    TestGroup("parseWhileStatement",
              TestGroup("good",
                        mkParseWhileSuccess("while 1 + 2 do writeInt 3 ; end",
                                            While(Op("+", Num("1"), Num("2")),
                                                  StatementSeq(WriteInt(Num("3"))))),
                        mkParseWhileSuccess("while 1 + 2 do end", While(Op("+", Num("1"), Num("2")),
                                                                        StatementSeq()))
              ),
              TestGroup("bad",
                        mkParseWhileFailure[While,BadMatchError]("while do writeInt 1 ;"),
                        mkParseWhileFailure[While,BadMatchError]("while 1 + 2 writeInt 3 ;"),
                        mkParseWhileFailure[While,EOFError]("while 1 + 2 do writeInt 3 ;"),
                        mkParseWhileFailure[While,EOFError]("while 1 + 2 do"),
                        mkParseWhileFailure[While,EOFError]("while 1 + 2"),
                        mkParseWhileFailure[While,EOFError]("while")
              )
    )

  /** [[Parser.parseWriteInt]] unit tests */
  val parseWriteIntTests =
    TestGroup("parseWriteInt",
              TestGroup("good",
                        mkParseWriteIntSuccess("writeInt 1 + 2", WriteInt(Op("+", Num("1"), Num("2"))))
              ),
              TestGroup("bad",
                        mkParseWriteIntFailure[WriteInt,EOFError]("writeInt")
              )
    )

  /** [[Parser.parseExpression]] unit tests */
  val parseExprTests =
    TestGroup("parseExpression",
              TestGroup("good",
                        mkParseExprSuccess("1", Num("1")),
                        mkParseExprSuccess("1 = 2", Op("=", Num("1"), Num("2"))),
                        mkParseExprSuccess("1 != 2", Op("!=", Num("1"), Num("2"))),
                        mkParseExprSuccess("1 < 2", Op("<", Num("1"), Num("2"))),
                        mkParseExprSuccess("1 > 2", Op(">", Num("1"), Num("2"))),
                        mkParseExprSuccess("1 <= 2", Op("<=", Num("1"), Num("2"))),
                        mkParseExprSuccess("1 >= 2", Op(">=", Num("1"), Num("2"))),
                        mkParseExprSuccess("1 * 2 = 3", Op("=", Op("*", Num("1"), Num("2")), Num("3"))),
                        mkParseExprSuccess("1 = 2 * 3", Op("=", Num("1"), Op("*", Num("2"), Num("3")))),
                        mkParseExprSuccess("1 * 2 = 3 * 4", Op("=", Op("*", Num("1"), Num("2")),
                                                               Op("*", Num("3"), Num("4")))),
                        mkParseExprSuccess("1 = 2 + 3",
                                           Op("=", Num("1"), Op("+", Num("2"), Num("3")))),
                        mkParseExprSuccess("1 + 2 = 3", Op("=", Op("+", Num("1"), Num("2")), Num("3"))),
                        mkParseExprSuccess("1 + 2 * 3 = 4 + 5 * 6",
                                           Op("=", Op("+", Num("1"), Op("*", Num("2"), Num("3"))),
                                              Op("+", Num("4"), Op("*", Num("5"), Num("6"))))),
                        mkParseExprSuccess("1 * 2 + 3 = 4 * 5 + 6",
                                           Op("=", Op("+", Op("*", Num("1"), Num("2")), Num("3")),
                                              Op("+", Op("*", Num("4"), Num("5")), Num("6")))),
                        mkParseExprSuccess("1 * 2 + 3 * 4", Op("+", Op("*", Num("1"), Num("2")),
                                                               Op("*", Num("3"), Num("4")))),
                        mkParseExprSuccess("( 1 * 2 + 3 = 4 * 5 + 6 )",
                                           Op("=", Op("+", Op("*", Num("1"), Num("2")), Num("3")),
                                              Op("+", Op("*", Num("4"), Num("5")), Num("6")))),
                        mkParseExprSuccess("( 1 * 2 ) * 3", Op("*", Op("*", Num("1"), Num("2")), Num("3"))),
                        mkParseExprSuccess("1 * ( 2 * 3 )", Op("*", Num("1"), Op("*", Num("2"), Num("3")))),
                        mkParseExprSuccess("( 1 + 2 ) * 3", Op("*", Op("+", Num("1"), Num("2")), Num("3"))),
                        mkParseExprSuccess("( 1 + 2 ) * ( 3 + 4 ) = ( 5 + 6 ) * ( 7 + 8 )",
                                           Op("=",
                                              Op("*",
                                                 Op("+", Num("1"), Num("2")),
                                                 Op("+", Num("3"), Num("4"))),
                                              Op("*",
                                                 Op("+", Num("5"), Num("6")),
                                                 Op("+", Num("7"), Num("8"))))),
                        mkParseExprSuccess("1 =", Num("1"), List("=":_*))
              ),
              TestGroup("bad",
                        mkParseExprFailure[Expr,BadMatchError]("!"),
                        mkParseExprFailure[Expr,BadMatchError]("= 1 1")
              )
    )

  /** [[Parser.parseSimpleExpression]] unit tests */
  val parseSimpleExprTests =
    TestGroup("parseSimpleExpression",
              TestGroup("good",
                        mkParseSimpleExprSuccess("1", Num("1")),
                        mkParseSimpleExprSuccess("1 + 2", Op("+", Num("1"), Num("2"))),
                        mkParseSimpleExprSuccess("1 + 2 * 3", Op("+", Num("1"), Op("*", Num("2"), Num("3")))),
                        mkParseSimpleExprSuccess("1 * 2 + 3", Op("+", Op("*", Num("1"), Num("2")), Num("3"))),
                        mkParseSimpleExprSuccess("1 * 2 + 3 * 4", Op("+", Op("*", Num("1"), Num("2")),
                                                                     Op("*", Num("3"), Num("4")))),
                        mkParseSimpleExprSuccess("1 +", Num("1"), List("+":_*))
              ),
              TestGroup("bad",
                        mkParseSimpleExprFailure[Expr,BadMatchError]("+ 1 1")
              )
    )

  /** [[Parser.parseTerm]] unit tests */
  val parseTermTests =
    TestGroup("parseTerm",
              TestGroup("good",
                        mkParseTermSuccess("1", Num("1")),
                        mkParseTermSuccess("1 * 2", Op("*", Num("1"), Num("2"))),
                        mkParseTermSuccess("1 div 2", Op("div", Num("1"), Num("2"))),
                        mkParseTermSuccess("1 mod 2", Op("mod", Num("1"), Num("2"))),
                        mkParseTermSuccess("1 *", Num("1"), List(Token("*",0,0)))
              ),
              TestGroup("bad",
                        mkParseTermFailure[Expr,BadMatchError]("* 1 1")
              )
    )

  /** [[Parser.parseFactor]] unit tests */
  val parseFactorTests =
    TestGroup("parseFactor",
              TestGroup("num",
                        mkParseFactorSuccess("0", Num("0")),
                        mkParseFactorSuccess("10", Num("10"))
              ),
              TestGroup("boollit",
                        mkParseFactorSuccess("false", BoolLit("false")),
                        mkParseFactorSuccess("true", BoolLit("true"))
              ),
              TestGroup("ident",
                        mkParseFactorSuccess("A", Ident("A")),
                        mkParseFactorSuccess("A0", Ident("A0"))
              ),
              TestGroup("expression",
                        mkParseFactorSuccess("( 1 * 2 )", Op("*", Num("1"), Num("2"))),
                        mkParseFactorSuccess("( 1 * 2 + 3 = 4 * 5 + 6 )",
                                             Op("=", Op("+", Op("*", Num("1"), Num("2")), Num("3")),
                                                Op("+", Op("*", Num("4"), Num("5")), Num("6"))))
              ),
              TestGroup("invalid",
                        mkParseFactorFailure[Expr,BadMatchError]("a"),
                        mkParseFactorFailure[Expr,BadMatchError]("0a"),
                        mkParseFactorFailure[Expr,BadMatchError]("1("),
                        mkParseFactorFailure[Expr,BadMatchError]("1)"),
                        mkParseFactorFailure[Expr,BadMatchError]("(1"),
                        mkParseFactorFailure[Expr,BadMatchError](")1"),
                        mkParseFactorFailure[Expr,BadMatchError]("A_"),
                        mkParseFactorFailure[Expr,BadMatchError]("False"),
                        mkParseFactorFailure[Expr,BadMatchError]("True"),
                        mkParseFactorFailure[Expr,BadMatchError]("("),
                        mkParseFactorFailure[Expr,BadMatchError]("( 1 * 1")
              )
    )

  /** All Parser unit tests */
  val tests =
    TestGroup("Parser",
              prewalkTests,
              parseProgramTests,
              parseDeclsTests,
              parseStmtSeqTests,
              parseStmtTests,
              parseAsgnTests,
              parseIfTests,
              parseElseTests,
              parseWhileTests,
              parseWriteIntTests,
              parseExprTests,
              parseSimpleExprTests,
              parseTermTests,
              parseFactorTests)

  /** Implicitly converts a string into a list of [[Scanner.Token]]s */
  private implicit def toTokenList(s: String): List[Token] = s.split("\\s+").toList.map{ Token(_,0,0) }

  // helper functions for successful parses

  private def mkParseSuccess[A](
    parse: Parser[A],
    input: String,
    expected: Node,
    remaining: List[Token] = List()
  ) : Test = Test(input, () => assertEqual(parse(input), Right((expected, remaining))))

  private def mkParseProgramSuccess(input: String, expected: Program, remaining: List[Token] = List()) : Test =
    mkParseSuccess(parseProgram, input, expected, remaining)

  private def mkParseDeclsSuccess(input: String, expected: Node, remaining: List[Token] = List()): Test =
    mkParseSuccess(parseDeclarations, input, expected, remaining)

  private def mkParseStmtSeqSuccess(input: String, expected: Node, remaining: List[Token] = List()): Test =
    mkParseSuccess(parseStatementSeq, input, expected, remaining)

  private def mkParseStmtSuccess(input: String, expected: Node, remaining: List[Token] = List()): Test =
    mkParseSuccess(parseStatement, input, expected, remaining)

  private def mkParseAsgnSuccess(input: String, expected: Node, remaining: List[Token] = List()): Test =
    mkParseSuccess(parseAssignment, input, expected, remaining)

  private def mkParseIfSuccess(input: String, expected: Node, remaining: List[Token] = List()): Test =
    mkParseSuccess(parseIfStatement, input, expected, remaining)

  private def mkParseElseSuccess(input: String, expected: Node, remaining: List[Token] = List()): Test =
    mkParseSuccess(parseElseClause, input, expected, remaining)

  private def mkParseWhileSuccess(input: String, expected: Node, remaining: List[Token] = List()): Test =
    mkParseSuccess(parseWhileStatement, input, expected, remaining)

  private def mkParseWriteIntSuccess(input: String, expected: Node, remaining: List[Token] = List()): Test =
    mkParseSuccess(parseWriteInt, input, expected, remaining)

  private def mkParseExprSuccess(input: String, expected: Node, remaining: List[Token] = List()): Test =
    mkParseSuccess(parseExpression, input, expected, remaining)

  private def mkParseSimpleExprSuccess(input: String, expected: Node, remaining: List[Token] = List()): Test =
    mkParseSuccess(parseSimpleExpression, input, expected, remaining)

  private def mkParseTermSuccess(input: String, expected: Node, remaining: List[Token] = List()): Test =
    mkParseSuccess(parseTerm, input, expected, remaining)

  private def mkParseFactorSuccess(input: String, expected: Node, remaining: List[Token] = List()): Test =
    mkParseSuccess(parseFactor, input, expected, remaining)

  // helper functions for failed parses

  private def mkParseFailure[A,E](parse: Parser[A], input: String)(implicit m: Manifest[E]) = {
    val clazz = m.erasure.asInstanceOf[Class[E]]
    Test(input,
         () => parse(input) match {
           case Left(e) if e.getClass == clazz => true
           case x                              =>
             throw new AssertionFailedError("expected %s, got %s".format(clazz, x))
         })
  }

  private def mkParseProgramFailure[A,E](input: String)(implicit m: Manifest[E]) =
    mkParseFailure(parseProgram, input)

  private def mkParseDeclsFailure[A,E](input: String)(implicit m: Manifest[E]) =
    mkParseFailure(parseDeclarations, input)

  private def mkParseStmtSeqFailure[A,E](input: String)(implicit m: Manifest[E]) =
    mkParseFailure(parseStatementSeq, input)

  private def mkParseStmtFailure[A,E](input: String)(implicit m: Manifest[E]) =
    mkParseFailure(parseStatement, input)

  private def mkParseAsgnFailure[A,E](input: String)(implicit m: Manifest[E]) =
    mkParseFailure(parseAssignment, input)

  private def mkParseIfFailure[A,E](input: String)(implicit m: Manifest[E]) =
    mkParseFailure(parseIfStatement, input)

  private def mkParseElseFailure[A,E](input: String)(implicit m: Manifest[E]) =
    mkParseFailure(parseElseClause, input)

  private def mkParseWhileFailure[A,E](input: String)(implicit m: Manifest[E]) =
    mkParseFailure(parseWhileStatement, input)

  private def mkParseWriteIntFailure[A,E](input: String)(implicit m: Manifest[E]) =
    mkParseFailure(parseWriteInt, input)

  private def mkParseExprFailure[A,E](input: String)(implicit m: Manifest[E]) =
    mkParseFailure(parseExpression, input)

  private def mkParseSimpleExprFailure[A,E](input: String)(implicit m: Manifest[E]) =
    mkParseFailure(parseSimpleExpression, input)

  private def mkParseTermFailure[A,E](input: String)(implicit m: Manifest[E]) =
    mkParseFailure(parseTerm, input)

  private def mkParseFactorFailure[A,E](input: String)(implicit m: Manifest[E]) =
    mkParseFailure(parseFactor, input)

}
