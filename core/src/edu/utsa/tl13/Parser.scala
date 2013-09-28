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
  case class Assignment(ident: String, expr: Either[Expr, ReadInt]) extends Statement

  /** Represents a readInt */
  case class ReadInt extends Node

  /** Parses a [[Decls]]
    *
    * @param tokens Stream of tokens to parse
    * @return A [[Decls]] and the remaining tokens to parse
    * @throws [[ParseError]]
    */
  def parseDeclarations(tokens: Traversable[Token]): (Decls, Traversable[Token]) = {
    def aux(res: Vector[Decl], tokens: Traversable[Token]): Pair[Vector[Decl],Traversable[Token]] = {
      if (tokens.isEmpty || tokens.head.value != "var") {
        (res, tokens)
      } else {
        var tks = tokens.tail

        if (tks.isEmpty)
          throw new EOSError("<ident>", tokens.last)
        if (!tks.head.value.matches("[A-Z][A-Z0-9]*"))
          throw new ParseError("<ident>", tks.head)
        val value = tks.head.value
        tks = tks. tail

        if (tks.isEmpty)
          throw new EOSError("as", tokens.last)
        if (tks.head.value != "as")
          throw new ParseError("as", tks.head)
        tks = tks.tail

        if (tks.isEmpty)
          throw new EOSError("int or bool", tokens.last)
        if (!tks.head.value.matches("int|bool"))
          throw new ParseError("int or bool", tks.head)
        val typ = tks.head.value
        tks = tks.tail

        if (tks.isEmpty)
          throw new EOSError(";", tokens.last)
        if (tks.head.value != ";")
          throw new ParseError(";", tks.head)

        aux(res :+ Decl(value, typ), tks.tail)
      }
    }
    val (res, auxTokens) = aux(Vector[Decl](), tokens)
    (Decls(res:_*), auxTokens)
  }

  /** Parses a [[StatementSeq]]
    *
    * @param tokens Stream of tokens to parse
    * @return A [[StatementSeq]] and the remaining tokens to parse
    * @throws [[ParseError]]
    */
  def parseStatementSeq(tokens: Traversable[Token]): (StatementSeq, Traversable[Token]) = {
    def aux(res: Vector[Statement], tokens: Traversable[Token]): Pair[Vector[Statement],Traversable[Token]] = {
      tokens.toSeq match {
        case Seq() => (res, tokens)
        case _     =>
          try {
            val (stmt, stmtTokens) = parseStatement(tokens)
            stmtTokens.toSeq match {
              case Seq()                        => throw new EOSError(";", tokens.last)
              case Seq(x, _*) if x.value != ";" => throw new ParseError(";", x)
              case Seq(_, rest @ _*)            => aux(res :+ stmt, rest)
            }
          } catch {
            case e: ParseError => (res, tokens)
          }
      }
    }
    val (stmts, auxTokens) = aux(Vector[Statement](), tokens)
    (StatementSeq(stmts:_*), auxTokens)
  }

  /** Parses a [[Statement]]
    *
    * @param tokens Stream of tokens to parse
    * @return A [[Statement]] and the remaining tokens to parse
    * @throws [[ParseError]]
    */
  def parseStatement(tokens: Traversable[Token]): (Statement, Traversable[Token]) = {
    tokens.head.value match {
      case "if"                             => parseIfStatement(tokens)
      case "while"                          => parseWhileStatement(tokens)
      case "writeInt"                       => parseWriteInt(tokens)
      case v if v.matches("[A-Z][A-Z0-9]*") => parseAssignment(tokens)
      case _                                =>
        throw new ParseError("if, while, writeInt, or assignment", tokens.head)
    }
  }

  /** Parses an [[Assignment]]
    *
    * @param tokens Stream of tokens to parse
    * @return An [[Assignment]] and the remaining tokens to parse
    * @throws [[ParseError]]
    */
  def parseAssignment(tokens: Traversable[Token]): (Assignment, Traversable[Token]) = {
    tokens.toSeq match {
      case Seq(ident, _*) if !ident.value.matches("[A-Z][A-Z0-9]*") =>
        throw new ParseError("<ident>", ident)
      case Seq(_)                                                   =>
        throw new EOSError(":=", tokens.last)
      case Seq(_, e, _*) if e.value != ":="                         =>
        throw new ParseError(":=", e)
      case Seq(_, _)                                                =>
        throw new EOSError("<expression> or readInt", tokens.last)
      case Seq(ident, _, x, rest @ _*) if x.value == "readInt"      =>
        (Assignment(ident.value, Right(ReadInt())), rest)
      case Seq(ident, _, rest @ _*)                                 => {
        val (expr, tokens) = parseExpression(rest)
        (Assignment(ident.value, Left(expr)), tokens)
      }
    }
  }

  /** Parses an [[If]] statement
    *
    * @param tokens Stream of tokens to parse
    * @return An [[If]] and the remaining tokens to parse
    * @throws [[ParseError]]
    */
  def parseIfStatement(tokens: Traversable[Token]): (If, Traversable[Token]) = {
    assert(tokens.head.value == "if")

    if (tokens.tail.isEmpty)
      throw new EOSError("<expression>", tokens.head)

    val (expr, exprTokens) = parseExpression(tokens.tail)
    if (exprTokens.isEmpty)
      throw new EOSError("then", tokens.last)
    if (exprTokens.head.value != "then")
      throw new ParseError("then", exprTokens.head)

    val (stmts, stmtsTokens) = parseStatementSeq(exprTokens.tail)

    val (els, elsTokens) =
      stmtsTokens.toSeq match {
        case Seq()                           => throw new EOSError("else or end", tokens.last)
        case Seq(x, _*) if x.value == "else" => {
          val (ss, tks) = parseStatementSeq(stmtsTokens.tail)
          (Some(ss), tks)
        }
        case Seq(x, _*) if x.value == "end"  => (None, stmtsTokens)
        case Seq(x, _*)                      => throw new ParseError("else or end", x)
      }

    if (elsTokens.isEmpty)
      throw new EOSError("end", tokens.last)
    if (elsTokens.head.value != "end")
      throw new ParseError("end", elsTokens.head)

    (If(expr, stmts, els), elsTokens.tail)
  }

  /** Parses an else clause
    *
    * @param tokens Stream of tokens to parse
    * @return A [[StatementSeq]] and the remaining tokens to parse
    * @throws [[ParseError]]
    */
  def parseElseClause(tokens: Traversable[Token]): (StatementSeq, Traversable[Token]) = {
    assert(tokens.head.value == "else")
    if (tokens.tail.isEmpty)
      throw new EOSError("<statementSequence>", tokens.head)
    parseStatementSeq(tokens.tail)
  }

  /** Parses a while statement
    *
    * @param tokens Stream of tokens to parse
    * @return A [[While]] and the remaining tokens to parse
    * @throws [[ParseError]]
    */
  def parseWhileStatement(tokens: Traversable[Token]): (While, Traversable[Token]) = {
    assert(tokens.head.value == "while")

    if (tokens.tail.isEmpty)
      throw new EOSError("<expression>", tokens.head)

    val (expr, exprTokens) = parseExpression(tokens.tail)
    if (exprTokens.isEmpty)
      throw new EOSError("do", tokens.last)
    if (exprTokens.head.value != "do")
      throw new ParseError("do", exprTokens.head)

    val (stmts, stmtsTokens) = parseStatementSeq(exprTokens.tail)
    if (stmtsTokens.isEmpty)
      throw new EOSError("end", exprTokens.last)
    if (stmtsTokens.head.value != "end")
      throw new ParseError("end", stmtsTokens.head)

    (While(expr, stmts), stmtsTokens.tail)
  }

  /** Parses a writeInt
    *
    * @param tokens Stream of tokens to parse
    * @return A [[WriteInt]] and the remaining tokens to parse
    * @throws [[ParseError]]
    */
  def parseWriteInt(tokens: Traversable[Token]): (WriteInt, Traversable[Token]) = {
    assert(tokens.head.value == "writeInt")
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
