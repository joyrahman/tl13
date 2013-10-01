package edu.utsa.tl13

import edu.utsa.tl13
import Parser._
import Scanner._
import UnitTest._

/** [[Parser]] unit tests */
object ParserTests {

  /** [[Parser.parseProgram]] unit tests */
  val parseProgramTests =
    TestGroup("parseProgram",
              TestGroup("good",
                        mkParseProgramSuccess("program begin end",
                                              Program(Decls(), StatementSeq())),
                        mkParseProgramSuccess("program var A as int ; begin writeInt 1 ; end",
                                              Program(Decls(Decl("A", "int")),
                                                      StatementSeq(WriteInt(Num("1")))))
                      ),
              TestGroup("bad",
                        mkParseProgramFailure[ParseError]("1 begin end"),
                        mkParseProgramFailure[ParseError]("program 1 end"),
                        mkParseProgramFailure[ParseError]("program begin 1"),
                        mkParseProgramFailure[ParseError]("program begin end 1"),
                        mkParseProgramFailure[EOSError]("program begin"),
                        mkParseProgramFailure[EOSError]("program"),
                        Test("", () => assertThrows[EOSError](parseProgram(List())))
                      )
            )

  /** [[Parser.parseDeclarations]] unit tests */
  val parseDeclsTests =
    TestGroup("parseDeclarations",
              TestGroup("good",
                        mkParseDeclsSuccess("", Decls(), List(Token("",0,0))),
                        mkParseDeclsSuccess("1", Decls(), List(Token("1",0,0))),
                        mkParseDeclsSuccess("var A as int ;", Decls(Decl("A", "int"))),
                        mkParseDeclsSuccess("var A as int ; var B as bool ;",
                                            Decls(Decl("A", "int"), Decl("B", "bool"))),
                        mkParseDeclsSuccess("var A as int ; begin",
                                            Decls(Decl("A", "int")), List(Token("begin",0,0)))
                      ),
              TestGroup("bad",
                        mkParseDeclsFailure[ParseError]("var as int ;"),
                        mkParseDeclsFailure[ParseError]("var A int ;"),
                        mkParseDeclsFailure[ParseError]("var A as ;"),
                        mkParseDeclsFailure[ParseError]("var A as int 1"),
                        mkParseDeclsFailure[EOSError]("var A as int"),
                        mkParseDeclsFailure[EOSError]("var A as"),
                        mkParseDeclsFailure[EOSError]("var A"),
                        mkParseDeclsFailure[EOSError]("var")
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
                        mkParseStmtSuccess("A := 1 + 2", Assignment("A", Left(Op("+", Num("1"), Num("2"))))),
                        mkParseStmtSuccess("if 1 + 2 then end",
                                           If(Op("+", Num("1"), Num("2")), StatementSeq(), None)),
                        mkParseStmtSuccess("while 1 + 2 do end",
                                           While(Op("+", Num("1"), Num("2")), StatementSeq())),
                        mkParseStmtSuccess("writeInt 3", WriteInt(Num("3")))
                      ),
              TestGroup("bad",
                        mkParseStmtFailure[ParseError]("*")
                      )
            )

  /** [[Parser.parseAssignment]] unit tests */
  val parseAsgnTests =
    TestGroup("parseAssignment",
              TestGroup("good",
                        mkParseAsgnSuccess("A := readInt", Assignment("A", Right(ReadInt()))),
                        mkParseAsgnSuccess("A := 1 + 2", Assignment("A", Left(Op("+", Num("1"), Num("2")))))
                      ),
              TestGroup("bad",
                        mkParseAsgnFailure[ParseError]("A readInt"),
                        mkParseAsgnFailure[ParseError]("A 1 + 2"),
                        mkParseAsgnFailure[EOSError]("A :="),
                        mkParseAsgnFailure[EOSError]("A")
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
                        mkParseIfFailure[ParseError]("if then writeInt 3 ; end"),
                        mkParseIfFailure[ParseError]("if 1 + 2 then writeInt 3 ; 3 end"),
                        mkParseIfFailure[ParseError]("if 1 + 2 + then writeInt 3 ; end"),
                        mkParseIfFailure[EOSError]("if 1 + 2 then writeInt 3 ; else writeInt 4 ;"),
                        mkParseIfFailure[EOSError]("if 1 + 2 then writeInt 3 ; else"),
                        mkParseIfFailure[EOSError]("if 1 + 2 then writeInt 3 ;"),
                        mkParseIfFailure[EOSError]("if 1 + 2 then"),
                        mkParseIfFailure[EOSError]("if 1 + 2"),
                        mkParseIfFailure[EOSError]("if")
                      )
            )

  /** [[Parser.parseElseClause]] unit tests */
  val parseElseTests =
    TestGroup("parseElseClause",
              TestGroup("good",
                        mkParseElseSuccess("else writeInt 3 ;", StatementSeq(WriteInt(Num("3"))))),
              TestGroup("bad",
                        mkParseElseFailure[EOSError]("else")))

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
                        mkParseWhileFailure[ParseError]("while do writeInt 1 ;"),
                        mkParseWhileFailure[ParseError]("while 1 + 2 writeInt 3 ;"),
                        mkParseWhileFailure[EOSError]("while 1 + 2 do writeInt 3 ;"),
                        mkParseWhileFailure[EOSError]("while 1 + 2 do"),
                        mkParseWhileFailure[EOSError]("while 1 + 2"),
                        mkParseWhileFailure[EOSError]("while")
                      )
            )

  /** [[Parser.parseWriteInt]] unit tests */
  val parseWriteIntTests =
    TestGroup("parseWriteInt",
              TestGroup("good",
                        mkParseWriteIntSuccess("writeInt 1 + 2", WriteInt(Op("+", Num("1"), Num("2"))))
                      ),
              TestGroup("bad",
                        mkParseWriteIntFailure[EOSError]("writeInt")
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
                                                 Op("+", Num("7"), Num("8")))))
                      ),
              TestGroup("bad",
                        mkParseExprFailure[ParseError]("!"),
                        mkParseExprFailure[ParseError]("= 1 1"),
                        mkParseExprFailure[EOSError]("1 =")
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
                                                                     Op("*", Num("3"), Num("4"))))
                      ),
              TestGroup("bad",
                        mkParseSimpleExprFailure[ParseError]("+ 1 1"),
                        mkParseSimpleExprFailure[EOSError]("1 +")
                      )
            )

  /** [[Parser.parseTerm]] unit tests */
  val parseTermTests =
    TestGroup("parseTerm",
              TestGroup("good",
                        mkParseTermSuccess("1", Num("1")),
                        mkParseTermSuccess("1 * 2", Op("*", Num("1"), Num("2"))),
                        mkParseTermSuccess("1 div 2", Op("div", Num("1"), Num("2"))),
                        mkParseTermSuccess("1 mod 2", Op("mod", Num("1"), Num("2")))
                      ),
              TestGroup("bad",
                        mkParseTermFailure[ParseError]("* 1 1"),
                        mkParseTermFailure[EOSError]("1 *")
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
                        mkParseFactorFailure[ParseError]("a"),
                        mkParseFactorFailure[ParseError]("0a"),
                        mkParseFactorFailure[ParseError]("1("),
                        mkParseFactorFailure[ParseError]("1)"),
                        mkParseFactorFailure[ParseError]("(1"),
                        mkParseFactorFailure[ParseError](")1"),
                        mkParseFactorFailure[ParseError]("A_"),
                        mkParseFactorFailure[ParseError]("False"),
                        mkParseFactorFailure[ParseError]("True"),
                        mkParseFactorFailure[EOSError]("("),
                        mkParseFactorFailure[EOSError]("( 1 * 1")
                      )
            )

  /** All Parser unit tests */
  val tests =
    TestGroup("Parser",
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
  private implicit def toTokenList(s: String): List[Token] = s.split("\\s+").toList.map { Token(_,0,0) }

  // helper functions for failed parses

  private def mkParseSuccess(
    parse: Traversable[Token] => (Node, Traversable[Token]),
    input: String, expected: Node, remaining: List[Token] = List())
  : Test =
    Test(input, () => assertEqual(parse(input), (expected, remaining)))

  private def mkParseProgramSuccess(input: String, expected: Program) : Test =
    Test(input, () => assertEqual(parseProgram(input), expected))

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

  private def mkParseFailure[A <: ParseError](
    parse: Traversable[Token] => (Node, Traversable[Token]), input: String)(implicit m: Manifest[A])
  : Test =
    Test(input, () => assertThrows[A](parse(input)))

  private def mkParseProgramFailure[A <: ParseError](input: String)(implicit m: Manifest[A]) : Test =
    Test(input, () => assertThrows[A](parseProgram(input)))

  private def mkParseDeclsFailure[A <: ParseError](input: String) (implicit m: Manifest[A]) =
    mkParseFailure[A](parseDeclarations, input)

  private def mkParseStmtSeqFailure[A <: ParseError](input: String) (implicit m: Manifest[A]) =
    mkParseFailure[A](parseStatementSeq, input)

  private def mkParseStmtFailure[A <: ParseError](input: String) (implicit m: Manifest[A]) =
    mkParseFailure[A](parseStatement, input)

  private def mkParseAsgnFailure[A <: ParseError](input: String) (implicit m: Manifest[A]) =
    mkParseFailure[A](parseAssignment, input)

  private def mkParseIfFailure[A <: ParseError](input: String) (implicit m: Manifest[A]) =
    mkParseFailure[A](parseIfStatement, input)

  private def mkParseElseFailure[A <: ParseError](input: String) (implicit m: Manifest[A]) =
    mkParseFailure[A](parseElseClause, input)

  private def mkParseWhileFailure[A <: ParseError](input: String) (implicit m: Manifest[A]) =
    mkParseFailure[A](parseWhileStatement, input)

  private def mkParseWriteIntFailure[A <: ParseError](input: String) (implicit m: Manifest[A]) =
    mkParseFailure[A](parseWriteInt, input)

  private def mkParseExprFailure[A <: ParseError](input: String) (implicit m: Manifest[A]) =
    mkParseFailure[A](parseExpression, input)

  private def mkParseSimpleExprFailure[A <: ParseError](input: String) (implicit m: Manifest[A]) =
    mkParseFailure[A](parseSimpleExpression, input)

  private def mkParseTermFailure[A <: ParseError](input: String) (implicit m: Manifest[A]) =
    mkParseFailure[A](parseTerm, input)

  private def mkParseFactorFailure[A <: ParseError](input: String) (implicit m: Manifest[A]) =
    mkParseFailure[A](parseFactor, input)

}
