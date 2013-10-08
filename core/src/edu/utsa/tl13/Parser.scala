package edu.utsa.tl13

import edu.utsa.tl13
import Scanner._

/** Parser module */
object Parser {

  type TokenStream = Traversable[Token]

  /** Represents an error encountered during parsing
    *
    * @param expected The expected token
    * @param token The incorrect given token
    */
  class ParseError(msg: String) extends Exception(msg) {
    def this(expected: String, token: Token) =
      this("line: %d, column: %d: expected %s, got %s" .format(token.line, token.column, expected, token.value));
  }

  /** Represents a parse error where a token was expected, but none were available
    *
    * @param expected The expected token
    */
  class EOSError(expected: String) extends ParseError("expected: %s, got EOF".format(expected))

  /** Base class for every node in the AST */
  abstract class Node {
    /** Calls a function on every [[Node]] and sub-node
      *
      * @param acc Initial value that is passed to each function
      * @param f Function to call on each node. The function is passed a node and the current
      *          accumulated state
      * @return The accumulated state
      */
    def fold[A](acc: A)(f: (A, Node) => A): A
    /** Returns the child nodes of this node */
    def children: Traversable[Node]
    /** Returns the "value" of the node, if not already provided by a node member */
    def value: String
  }

  /** Represents an expression. Can be a num, boollit, ident, or operation */
  abstract class Expr extends Node

  /** Represents a statement. Can be an if, while, assignment, or writeInt */
  abstract class Statement extends Node

  /** Represents a number
    *
    * @param value The value of the number
    */
  case class Num(value: String) extends Expr {
    def fold[A](acc: A)(f: (A, Node) => A): A = f(acc, this)
    def children = Vector()
  }

  /** Represents a boolean literal
    *
    * @param value true or false
    */
  case class BoolLit(value: String) extends Expr {
    def fold[A](acc: A)(f: (A, Node) => A): A = f(acc, this)
    def children = Vector()
  }

  /** Represents an identifier
    *
    * @param value The value of the identifier
    */
  case class Ident(value: String) extends Expr {
    def fold[A](acc: A)(f: (A, Node) => A): A = f(acc, this)
    def children = Vector()
  }

  /** Represents an operation
    *
    * @param value The operation *, +, -, etc
    * @param left The left-hand side expression
    * @param right The right-hand side expresssion
    */
  case class Op(value: String, left: Expr, right: Expr) extends Expr {
    def fold[A](acc: A)(f: (A, Node) => A): A = {
      var res = f(acc, this)
      res = left.fold(res)(f)
      res = right.fold(res)(f)
      res
    }
    def children = Vector(left, right)
  }

  /** Represents a sequence of statements
    *
    * @param stmts The sequences
    */
  case class StatementSeq(stmts: Statement*) extends Node {
    def fold[A](acc: A)(f: (A, Node) => A): A =
      stmts.foldLeft( f(acc, this) ) { (a,n) => n.fold(a)(f) }
    def children = stmts
    def value = "stmt list"
  }

  /** Represents a type
    *
    * @param value the type
    */
  case class Type(value: String) extends Node {
    def fold[A](acc: A)(f: (A, Node) => A): A = f(acc, this)
    def children = Vector()
  }

  /** Represents a declaration
    *
    * @param value The value of the declaration
    * @param typ The [[Type]] of the declaration
    */
  case class Decl(value: String, typ: Type) extends Node {
    def fold[A](acc: A)(f: (A, Node) => A): A = {
      var res = f(acc, this)
      typ.fold(res)(f)
    }
    def children = Vector(typ)
  }

  /** Represents several declarations
    *
    * @param decls The declarations
    */
  case class Decls(decls: Decl*) extends Node {
    def fold[A](acc: A)(f: (A, Node) => A): A =
      decls.foldLeft( f(acc, this) ) { (a,n) => n.fold(a)(f) }
    def children = decls
    def value = "decl list"
  }

  /** Represents a program
    *
    * @param decls The program declarations
    * @param stmts The statements in the program
    */
  case class Program(decls: Decls, stmts: StatementSeq) extends Node {
    def fold[A](acc: A)(f: (A, Node) => A): A = {
      var res = f(acc, this)
      res = decls.fold(res)(f)
      res = stmts.fold(res)(f)
      res
    }
    def children = Vector(decls, stmts)
    def value = "program"
  }

  /** Represents a writeInt statement
    *
    * @param expr The expression to write
    */
  case class WriteInt(expr: Expr) extends Statement {
    def fold[A](acc: A)(f: (A, Node) => A): A = {
      val res = f(acc, this)
      expr.fold(res)(f)
    }
    def children = Vector(expr)
    def value = "writeInt"
  }

  /** Represents a while statement
    *
    * @param expr The condition that must be true
    * @param stmts The statements to execute
    */
  case class While(expr: Expr, stmts: StatementSeq) extends Statement {
    def fold[A](acc: A)(f: (A, Node) => A): A = {
      var res = f(acc, this)
      res = expr.fold(res)(f)
      res = stmts.fold(res)(f)
      res
    }
    def children = Vector(expr, stmts)
    def value = "while"
  }

  /** Represents an if statement
    *
    * @param expr The expression that is evaluated for truthiness
    * @param stmts The statements that are executed when true
    * @param els The statements executed when false
    */
  case class If(expr: Expr, stmts: StatementSeq, els: Option[StatementSeq]) extends Statement {
    def fold[A](acc: A)(f: (A, Node) => A): A = {
      var res = f(acc, this)
      res = expr.fold(res)(f)
      res = stmts.fold(res)(f)
      res = if (els.isEmpty) res else els.get.fold(res)(f)
      res
    }
    def children = {
      var v = Vector[Node](expr, stmts)
      if (els.isEmpty)
        v
      else
        v :+ els.get
    }
    def value = "if"
  }

  /** Represents an assignment
    *
    * @param ident The identifier being assigned
    * @param expr Either an expression to assign or readInt
    */
  case class Assignment(ident: Ident, expr: Either[Expr, ReadInt]) extends Statement {
    def fold[A](acc: A)(f: (A, Node) => A): A = {
      var res = f(acc, this)
      res = ident.fold(res)(f)
      res = expr match {
        case Right(x) => x.fold(res)(f)
        case Left(x)  => x.fold(res)(f)
      }
      res
    }
    def children = Vector(ident, if (expr.isLeft) expr.left.get else expr.right.get)
    def value = ":="
  }

  /** Represents a readInt */
  case class ReadInt extends Node {
    def fold[A](acc: A)(f: (A, Node) => A): A = f(acc, this)
    def children = Vector()
    def value = "readInt"
  }

  /** Parses a [[Program]]
    *
    * @param tokens Stream of tokens to parse
    * @return A [[Program]] and the remaining tokens to parse
    * @throws [[ParseError]]
    */
  def parseProgram(tokens: TokenStream): Program = {
    if (tokens.isEmpty)
      throw new EOSError("program")
    if (tokens.head.value != "program")
      throw new ParseError("program", tokens.head)

    val (decls, declTokens) = parseDeclarations(tokens.tail)
    if (declTokens.isEmpty)
      throw new EOSError("begin")
    if (declTokens.head.value != "begin")
      throw new ParseError("begin", declTokens.head)

    val (stmts, stmtsTokens) = parseStatementSeq(declTokens.tail)
    if (stmtsTokens.isEmpty)
      throw new EOSError("end")
    if (stmtsTokens.head.value != "end")
      throw new ParseError("end", stmtsTokens.head)
    if (!stmtsTokens.tail.isEmpty)
      throw new ParseError("<EOF>", stmtsTokens.tail.head)

    Program(decls, stmts)
  }

  /** Parses a [[Decls]]
    *
    * @param tokens Stream of tokens to parse
    * @return A [[Decls]] and the remaining tokens to parse
    * @throws [[ParseError]]
    */
  def parseDeclarations(tokens: TokenStream): (Decls, TokenStream) = {
    def aux(res: Vector[Decl], tokens: TokenStream): Pair[Vector[Decl],TokenStream] = {
      if (tokens.isEmpty || tokens.head.value != "var") {
        (res, tokens)
      } else {
        var tks = tokens.tail

        if (tks.isEmpty)
          throw new EOSError("<ident>")
        if (!tks.head.value.matches("[A-Z][A-Z0-9]*"))
          throw new ParseError("<ident>", tks.head)
        val value = tks.head.value
        tks = tks. tail

        if (tks.isEmpty)
          throw new EOSError("as")
        if (tks.head.value != "as")
          throw new ParseError("as", tks.head)
        tks = tks.tail

        if (tks.isEmpty)
          throw new EOSError("int or bool")
        val (typ, typTokens) = parseType(tks)
        tks = typTokens

        if (tks.isEmpty)
          throw new EOSError(";")
        if (tks.head.value != ";")
          throw new ParseError(";", tks.head)

        aux(res :+ Decl(value, typ), tks.tail)
      }
    }
    val (res, auxTokens) = aux(Vector[Decl](), tokens)
    (Decls(res:_*), auxTokens)
  }

  /** Parses a [[Type]]
    *
    * @param tokens Stream of tokens to parse
    * @return A [[Type]] and the remaining tokens to parse
    * @throws [[ParseError]]
    * @todo Unit Tests!
    */
  def parseType(tokens: TokenStream): (Type, TokenStream) = {
    tokens.head.value match {
      case "int"  => (Type("int"), tokens.tail)
      case "bool" => (Type("bool"), tokens.tail)
      case _      => throw new ParseError("int or bool", tokens.head)
    }
  }

  /** parses a [[StatementSeq]]
    *
    * @param tokens Stream of tokens to parse
    * @return A [[StatementSeq]] and the remaining tokens to parse
    * @throws [[ParseError]]
    */
  def parseStatementSeq(tokens: TokenStream): (StatementSeq, TokenStream) = {
    def aux(res: Vector[Statement], tokens: TokenStream): Pair[Vector[Statement],TokenStream] = {
      tokens.toSeq match {
        case Seq() => (res, tokens)
        case _     =>
          try {
            val (stmt, stmtTokens) = parseStatement(tokens)
            stmtTokens.toSeq match {
              case Seq()                        => throw new EOSError(";")
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
  def parseStatement(tokens: TokenStream): (Statement, TokenStream) = {
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
  def parseAssignment(tokens: TokenStream): (Assignment, TokenStream) = {
    tokens.toSeq match {
      case Seq(ident, _*) if !ident.value.matches("[A-Z][A-Z0-9]*") =>
        throw new ParseError("<ident>", ident)
      case Seq(_)                                                   =>
        throw new EOSError(":=")
      case Seq(_, e, _*) if e.value != ":="                         =>
        throw new ParseError(":=", e)
      case Seq(_, _)                                                =>
        throw new EOSError("<expression> or readInt")
      case Seq(ident, _, x, rest @ _*) if x.value == "readInt"      =>
        (Assignment(Ident(ident.value), Right(ReadInt())), rest)
      case Seq(ident, _, rest @ _*)                                 => {
        val (expr, tokens) = parseExpression(rest)
        (Assignment(Ident(ident.value), Left(expr)), tokens)
      }
    }
  }

  /** Parses an [[If]] statement
    *
    * @param tokens Stream of tokens to parse
    * @return An [[If]] and the remaining tokens to parse
    * @throws [[ParseError]]
    */
  def parseIfStatement(tokens: TokenStream): (If, TokenStream) = {
    assert(tokens.head.value == "if")

    if (tokens.tail.isEmpty)
      throw new EOSError("<expression>")

    val (expr, exprTokens) = parseExpression(tokens.tail)
    if (exprTokens.isEmpty)
      throw new EOSError("then")
    if (exprTokens.head.value != "then")
      throw new ParseError("then", exprTokens.head)

    val (stmts, stmtsTokens) = parseStatementSeq(exprTokens.tail)

    val (els, elsTokens) =
      stmtsTokens.toSeq match {
        case Seq()                           => throw new EOSError("else or end")
        case Seq(x, _*) if x.value == "else" => {
          val (ss, tks) = parseStatementSeq(stmtsTokens.tail)
          (Some(ss), tks)
        }
        case Seq(x, _*) if x.value == "end"  => (None, stmtsTokens)
        case Seq(x, _*)                      => throw new ParseError("else or end", x)
      }

    if (elsTokens.isEmpty)
      throw new EOSError("end")
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
  def parseElseClause(tokens: TokenStream): (StatementSeq, TokenStream) = {
    assert(tokens.head.value == "else")
    if (tokens.tail.isEmpty)
      throw new EOSError("<statementSequence>")
    parseStatementSeq(tokens.tail)
  }

  /** Parses a while statement
    *
    * @param tokens Stream of tokens to parse
    * @return A [[While]] and the remaining tokens to parse
    * @throws [[ParseError]]
    */
  def parseWhileStatement(tokens: TokenStream): (While, TokenStream) = {
    assert(tokens.head.value == "while")

    if (tokens.tail.isEmpty)
      throw new EOSError("<expression>")

    val (expr, exprTokens) = parseExpression(tokens.tail)
    if (exprTokens.isEmpty)
      throw new EOSError("do")
    if (exprTokens.head.value != "do")
      throw new ParseError("do", exprTokens.head)

    val (stmts, stmtsTokens) = parseStatementSeq(exprTokens.tail)
    if (stmtsTokens.isEmpty)
      throw new EOSError("end")
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
  def parseWriteInt(tokens: TokenStream): (WriteInt, TokenStream) = {
    assert(tokens.head.value == "writeInt")
    if (tokens.tail.isEmpty) {
      throw new EOSError("<expression>")
    } else {
      val (expr, exprTokens) = parseExpression(tokens.tail)
      (WriteInt(expr), exprTokens)
    }
  }

  private def parseExprAux(
    tokens: TokenStream, pattern: String, expected: String,
    parse: TokenStream => (Expr, TokenStream))
  : (Expr, TokenStream) = {
    val (left, leftTokens) = parse(tokens)
    leftTokens.toSeq match {
      case Seq()                                     => (left, leftTokens)
      case Seq(op, _*) if !op.value.matches(pattern) => (left, leftTokens)
      case Seq(op)                                   => throw new EOSError(expected)
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
  def parseExpression(tokens: TokenStream): (Expr, TokenStream) =
    parseExprAux(tokens, "=|!=|<|>|<=|>=", "<simpleExpression>", parseSimpleExpression)

  /** Parses a simpleExpression
    *
    * @param tokens The tokens to parse
    * @return A parsed [[Expr]] and the rest of the [[Scanner.Token]] stream
    * @throws [[ParseError]]
    */
  def parseSimpleExpression(tokens: TokenStream): (Expr, TokenStream) =
    parseExprAux(tokens, "\\+|\\-", "<term>", parseTerm)

  /** Parses a term
    *
    * @param tokens The tokens to parse
    * @return A parsed [[Expr]] and the rest of the [[Scanner.Token]] stream
    * @throws [[ParseError]]
    */
  def parseTerm(tokens: TokenStream): (Expr, TokenStream) =
    parseExprAux(tokens, "\\*|div|mod", "<factor>", parseFactor)

  /** Parses a factor
    *
    * @param tokens The tokens to parse
    * @return A parsed [[Expr]] and the rest of the [[Scanner.Token]] stream
    * @throws [[ParseError]]
    */
  def parseFactor(tokens: TokenStream): (Expr, TokenStream) = {
    tokens.head.value match {
      case v if v.matches("[1-9][0-9]*|0")  => (Num(v), tokens.tail)
      case v if v.matches("false|true")     => (BoolLit(v), tokens.tail)
      case v if v.matches("[A-Z][A-Z0-9]*") => (Ident(v), tokens.tail)
      case "("                              => {
        if (tokens.tail.isEmpty)
          throw new EOSError("<expression>")

        val (expr, exprTokens) = parseExpression(tokens.tail)
        exprTokens.toSeq match {
          case Seq()                        => throw new EOSError(")")
          case Seq(x, _*) if x.value != ")" => throw new ParseError(")", x)
          case Seq(x, rest @ _*)            => (expr, rest)
        }
      }
      case _                                =>
        throw new ParseError("num, false, true, identifier, or (", tokens.head)
    }
  }

}
