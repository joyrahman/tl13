package edu.utsa.tl13

import edu.utsa.tl13
import Scanner._

/** Parser module */
object Parser {

  /** Represents an error encountered during parsing
    *
    * @param expected The expected token
    * @param token The incorrect given token
    */
  class ParseError(expected: String, token: Token)
    extends Exception("line: %d, column: %d: expected %s, got %s"
                      .format(token.line, token.column, expected, token.value))

  /** Represents a parse error where a token was expected, but none were available
    *
    * @param expected The expected token
    * @param lastToken The last token given before the end of stream
    */
  class EOSError(expected: String, lastToken: Token)
    extends ParseError(expected, lastToken.copy(value="\"\"",
                                                column=lastToken.column + lastToken.value.length + 1))

  /** Base class for every node in the AST */
  abstract class Node

  /** Represents an expression. Can be a num, boollit, ident, or operation */
  abstract class Expr extends Node

  /** Represents a statement. Can be an if, while, assignment, or writeInt */
  abstract class Statement extends Node

  /** Represents a number
    *
    * @param value The value of the number
    */
  case class Num(value: String) extends Expr

  /** Represents a boolean literal
    *
    * @param value true or false
    */
  case class BoolLit(value: String) extends Expr

  /** Represents an identifier
    *
    * @param value The value of the identifier
    */
  case class Ident(value: String) extends Expr

  /** Represents an operation
    *
    * @param value The operation *, +, -, etc
    * @param left The left-hand side expression
    * @param right The right-hand side expresssion
    */
  case class Op(value: String, left: Expr, right: Expr) extends Expr

  /** Represents a sequence of statements
    *
    * @param stmts The sequences
    */
  case class StatementSeq(stmts: Statement*) extends Node

  /** Represents a declaration
    *
    * @param value The value of the declaration
    * @param typ The type of the declaration
    */
  case class Decl(value: String, typ: String) extends Node

  /** Represents several declarations
    *
    * @param decls The declarations
    */
  case class Decls(decls: Decl*) extends Node

  /** Represents a program
    *
    * @param decls The program declarations
    * @param stmts The statements in the program
    */
  case class Program(decls: Decls, stmts: StatementSeq) extends Node

  /** Represents a writeInt statement
    *
    * @param expr The expression to write
    */
  case class WriteInt(expr: Expr) extends Statement

  /** Represents a while statement
    *
    * @param expr The condition that must be true
    * @param stmts The statements to execute
    */
  case class While(expr: Expr, stmts: StatementSeq) extends Statement

  /** Represents an if statement
    *
    * @param expr The expression that is evaluated for truthiness
    * @param stmts The statements that are executed when true
    * @param els The statements executed when false
    */
  case class If(expr: Expr, stmts: StatementSeq, els: Option[StatementSeq]) extends Statement

  /** Represents an assignment
    *
    * @param ident The identifier being assigned
    * @param expr Either an expression to assign or readInt
    */
  case class Assignment(ident: String, expr: Option[Expr]) extends Statement

  /** Parses a writeInt
    *
    * @param tokens Stream of tokens to parse
    * @return A [[WriteInt]] and the remaining tokens to parse
    */
  def parseWriteInt(tokens: Traversable[Token]): (WriteInt, Traversable[Token]) = {
    if (tokens.tail.isEmpty) {
      throw new EOSError("<expression>", tokens.head)
    } else {
      val (expr, exprTokens) = parseExpression(tokens.tail)
      (WriteInt(expr), exprTokens)
    }
  }

  private def parseExprAux(
    tokens: Traversable[Token], pattern: String, expected: String,
    parse: Traversable[Token] => (Expr, Traversable[Token]))
  : (Expr, Traversable[Token]) = {
    val (left, leftTokens) = parse(tokens)
    leftTokens.toSeq match {
      case Seq()                                     => (left, leftTokens)
      case Seq(op, _*) if !op.value.matches(pattern) => (left, leftTokens)
      case Seq(op)                                   => throw new EOSError(expected, op)
      case Seq(op, rest @ _*)                        => {
        val (right, righTokens) = parse(rest)
        (Op(op.value, left, right), righTokens)
      }
    }
  }

  /** Parses an expresstion
    *
    * @param tokens The tokens to parse
    * @return A parsed [[Expr]] and the rest of the [[Scanner.Token]] stream
    * @throws [[ParseError]]
    */
  def parseExpression(tokens: Traversable[Token]): (Expr, Traversable[Token]) =
    parseExprAux(tokens, "=|!=|<|>|<=|>=", "<simpleExpression>", parseSimpleExpression)

  /** Parses a simpleExpression
    *
    * @param tokens The tokens to parse
    * @return A parsed [[Expr]] and the rest of the [[Scanner.Token]] stream
    * @throws [[ParseError]]
    */
  def parseSimpleExpression(tokens: Traversable[Token]): (Expr, Traversable[Token]) =
    parseExprAux(tokens, "\\+|\\-", "<term>", parseTerm)

  /** Parses a term
    *
    * @param tokens The tokens to parse
    * @return A parsed [[Expr]] and the rest of the [[Scanner.Token]] stream
    * @throws [[ParseError]]
    */
  def parseTerm(tokens: Traversable[Token]): (Expr, Traversable[Token]) =
    parseExprAux(tokens, "\\*|div|mod", "<factor>", parseFactor)

  /** Parses a factor
    *
    * @param tokens The tokens to parse
    * @return A parsed [[Expr]] and the rest of the [[Scanner.Token]] stream
    * @throws [[ParseError]]
    */
  def parseFactor(tokens: Traversable[Token]): (Expr, Traversable[Token]) = {
    tokens.head.value match {
      case v if v.matches("[1-9][0-9]*|0")  => (Num(v), tokens.tail)
      case v if v.matches("false|true")     => (BoolLit(v), tokens.tail)
      case v if v.matches("[A-Z][A-Z0-9]*") => (Ident(v), tokens.tail)
      case "("                              => {
        if (tokens.tail.isEmpty)
          throw new EOSError("<expression>", tokens.head)

        val (expr, exprTokens) = parseExpression(tokens.tail)
        exprTokens.toSeq match {
          case Seq()                        => throw new EOSError(")", tokens.last)
          case Seq(x, _*) if x.value != ")" => throw new ParseError(")", x)
          case Seq(x, rest @ _*)            => (expr, rest)
        }
      }
      case _                                =>
        throw new ParseError("num, false, true, identifier, or (", tokens.head)
    }
  }

}
