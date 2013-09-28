package edu.utsa.tl13

import edu.utsa.tl13
import Parser._
import Scanner._
import UnitTest._

/** [[Parser]] unit tests */
object ParserTests {

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
