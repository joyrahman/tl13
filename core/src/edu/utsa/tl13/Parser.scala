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

}
