package edu.utsa.tl13

import Scan._

/** Parser module */
object Parse {

  /** Function which parses a [[Scan.TokenStream]] and returns an error or the
    * result of the parse and remaining tokens
    */
  type Parser[A] = TokenStream => Either[ParseError, (A, TokenStream)]

  /** Base class for parsing errors */
  sealed abstract class ParseError {
    def expected: Set[String]
  }

  /** Represents an error where a token was encounted which wasn't expected */
  case class BadMatchError(expected: Set[String], token: Token) extends ParseError

  /** Represents an error where a [[Scan.Token]] was expected but none were available */
  case class EOFError(expected: Set[String]) extends ParseError

  /** Base class for every node in the AST */
  abstract class Node {
    /** Calls a function on every [[Parse.Node]] and sub-node in a pre-order traversal
      *
      * @param acc Initial value that is passed to each function
      * @param f Function to call on each node. The function is passed a node and the current
      *          accumulated state
      * @return The accumulated state
      */
    def prewalk[A](acc: A)(f: (A, Node) => A): A = {
      def aux(acc: A, cs: Seq[Node]): A = {
        if (cs.isEmpty) acc
        else            aux(cs.head.prewalk(acc)(f), cs.tail)
      }
      aux(f(acc,this), children)
    }
    /** Calls a function on every [[Parse.Node]] and sub-node in a post-order traversal
      *
      * @param acc Initial value that is passed to each function
      * @param f Function to call on each node. The function is passed a node and the current
      *          accumulated state
      * @return The accumulated state
      */
    def postwalk[A](acc: A)(f: (A, Node) => A): A = {
      def aux(acc: A, cs: Seq[Node]): A = {
        if (cs.isEmpty) acc
        else            aux(cs.head.postwalk(acc)(f), cs.tail)
      }
      f(aux(acc, children), this)
    }
    /** Returns the child nodes of this node */
    def children: Seq[Node]
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
    def children = Vector()
  }

  /** Represents a boolean literal
    *
    * @param value true or false
    */
  case class BoolLit(value: String) extends Expr {
    def children = Vector()
  }

  /** Represents an identifier
    *
    * @param value The value of the identifier
    */
  case class Ident(value: String) extends Expr {
    def children = Vector()
  }

  /** Represents an operation
    *
    * @param value The operation *, +, -, etc
    * @param left The left-hand side expression
    * @param right The right-hand side expresssion
    */
  case class Op(value: String, left: Expr, right: Expr) extends Expr {
    def children = Vector(left, right)
  }

  /** Represents a sequence of statements
    *
    * @param stmts The sequences
    */
  case class StatementSeq(stmts: Statement*) extends Node {
    def children = stmts
    def value = "stmt list"
  }

  /** Represents a type */
  abstract class Type extends Node {
    def children = Vector()
  }

  object Type {
    /** Creates a type from a string */
    def apply(s: String) = s match {
        case "int" => TL13Int()
        case "bool" => TL13Bool()
        case "none" => TL13None()
      }
  }

  /** Represents an int in TL13 */
  case class TL13Int extends Type {
    def value = "int"
  }

  /** Represents a bool in TL13 */
  case class TL13Bool extends Type {
    def value = "bool"
  }

  /** Represents a node with no type */
  case class TL13None extends Type {
    def value = "none"
  }

  /** Represents a declaration
    *
    * @param value The value of the declaration
    * @param typ The [[Type]] of the declaration
    */
  case class Decl(value: String, typ: Type) extends Node {
    def children = Vector(typ)
  }

  /** Represents several declarations
    *
    * @param decls The declarations
    */
  case class Decls(decls: Decl*) extends Node {
    def children = decls
    def value = "decl list"
  }

  /** Represents a program
    *
    * @param decls The program declarations
    * @param stmts The statements in the program
    */
  case class Program(decls: Decls, stmts: StatementSeq) extends Node {
    def children = Vector(decls, stmts)
    def value = "program"
  }

  /** Represents a writeInt statement
    *
    * @param expr The expression to write
    */
  case class WriteInt(expr: Expr) extends Statement {
    def children = Vector(expr)
    def value = "writeInt"
  }

  /** Represents a while statement
    *
    * @param expr The condition that must be true
    * @param stmts The statements to execute
    */
  case class While(expr: Expr, stmts: StatementSeq) extends Statement {
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
    def children = {
      var v = Vector[Node](expr, stmts)
      if (els.isEmpty)
        v
      else
        v :+ els.get
    }
    def value = "if"
  }

  /** Represents an assignment */
  sealed abstract class Assignment extends Statement

  /** Represents an assignment of an expression to an identifier
    *
    * @param ident The identifier
    * @param expr The expression
    */
  case class AsgnExpr(ident: Ident, expr: Expr) extends Assignment {
    def children = Vector(ident, expr)
    def value = ":="
  }

  /** Represents an assignment to an identifier from a readInt
    *
    * @param ident The identifier
    */
  case class ReadInt(ident: Ident) extends Assignment {
    def children = Vector(ident)
    def value = ":= readInt"
  }

  /** [[Parser]] which parses a [[Program]] */
  def parseProgram: Parser[Program] =
    tokens => for {
      p <- parseRegex("program")(tokens).right
      d <- parseDeclarations(p._2).right
      b <- parseRegex("begin")(d._2).right
      s <- parseStatementSeq(b._2).right
      e <- parseEnd(s._2).right
      f <- parseEOF(e._2).right
    } yield (Program(d._1, s._1), f._2)

  /** [[Parser]] which parses a [[Decls]] */
  def parseDeclarations: Parser[Decls] =
    tokens => for {
      d <- many(parseDeclaration)(tokens).right
    } yield (Decls(d._1:_*), d._2)

  /** [[Parser]] which parses a [[Type]] */
  def parseType: Parser[Type] =
    tokens => for {
      t <- parseRegex("int|bool")(tokens).right
    } yield (Type(t._1), t._2)

  /** [[Parser]] which parses a [[StatementSeq]] */
  def parseStatementSeq: Parser[StatementSeq] =
    tokens => for {
      ss <- many(tokens => for {
                   s <- parseStatement(tokens).right
                   c <- parseSC(s._2).right
                 } yield (s._1, c._2))(tokens).right
    } yield (StatementSeq(ss._1:_*), ss._2)

  /** [[Parser]] which parses a [[Statement]] */
  def parseStatement: Parser[Statement] =
    choice(parseAssignment, parseIfStatement, parseWhileStatement, parseWriteInt)

  /** [[Parser]] which parses an [[Assignment]] */
  def parseAssignment: Parser[Assignment] =
    tokens => for {
      i <- parseIdent(tokens).right
      a <- parseRegex(":=")(i._2).right
      e <- choice(parseExpression, parseRegex("readInt"))(a._2).right
    } yield e._1 match {
      case "readInt" => (ReadInt(i._1), e._2)
      case _         => (AsgnExpr(i._1, e._1.asInstanceOf[Expr]), e._2)
    }

  /** [[Parser]] which parses an [[If]] */
  def parseIfStatement: Parser[If] =
    tokens => for {
      i <- parseRegex("if")(tokens).right
      e <- parseExpression(i._2).right
      t <- parseRegex("then")(e._2).right
      s <- parseStatementSeq(t._2).right
      n <- choice(tokens => for {
                    e <- parseElseClause(tokens).right
                    n <- parseEnd(e._2).right
                  } yield (e._1, n._2),
                  parseEnd)(s._2).right
    } yield (If(e._1, s._1, if (n._1 == "end") None else Some(n._1.asInstanceOf[StatementSeq])), n._2)

  /** [[Parser]] which parses an else clause */
  def parseElseClause: Parser[StatementSeq] =
    tokens => for {
      e <- parseRegex("else")(tokens).right
      s <- parseStatementSeq(e._2).right
    } yield (s._1, s._2)

  /** [[Parser]] which parses a [[While]] */
  def parseWhileStatement: Parser[While] =
    tokens => for {
      w <- parseRegex("while")(tokens).right
      e <- parseExpression(w._2).right
      d <- parseRegex("do")(e._2).right
      s <- parseStatementSeq(d._2).right
      n <- parseRegex("end")(s._2).right
    } yield (While(e._1, s._1), n._2)


  /** [[Parser]] which parses a [[WriteInt]] */
  def parseWriteInt: Parser[WriteInt] =
    tokens => for {
      w <- parseRegex("writeInt")(tokens).right
      e <- parseExpression(w._2).right
    } yield (WriteInt(e._1), e._2)

  /** [[Parser]] which parses an expression */
  def parseExpression: Parser[Expr] = parseExprAux(parseSimpleExpression, "=|!=|<|>|<=|>=")

  /** [[Parser]] which parses a simple expression */
  def parseSimpleExpression: Parser[Expr] = parseExprAux(parseTerm, "\\+|\\-")

  /** [[Parser]] which parses a term */
  def parseTerm: Parser[Expr] = parseExprAux(parseFactor, "\\*|div|mod")

  /** [[Parser]] which parses [[BoolLit]], [[Num]], [[Ident]], or "( [[Expr]] )" */
  def parseFactor: Parser[Expr] =
    tokens => choice(parseBoolLit, parseNum, parseIdent, parseParens)(tokens)

  /** [[Parser]] which parses "end" */
  private def parseEnd: Parser[String] = parseRegex("end")

  /** [[Parser]] which parses a semi-colon */
  private def parseSC: Parser[String] = parseRegex(";")

  /** [[Parser]] which parses End of File */
  private def parseEOF: Parser[Unit] =
    tokens => tokens match {
      case Seq() => Right((Unit, tokens))
      case _     => Left(BadMatchError(Set("EOF"), tokens.head))
    }

  /** [[Parser]] which parses a [[Decl]] */
  private def parseDeclaration: Parser[Decl] =
    tokens => for {
      v <- parseRegex("var")(tokens).right
      i <- parseIdent(v._2).right
      a <- parseRegex("as")(i._2).right
      t <- parseType(a._2).right
      s <- parseSC(t._2).right
    } yield (Decl(i._1.value, t._1), s._2)


  /** [[Parser]] which parses "( [[Expr]] )" */
  private def parseParens: Parser[Expr] =
    tokens => for {
      l <- parseRegex("\\(")(tokens).right
      e <- parseExpression(l._2).right
      r <- parseRegex("\\)")(e._2).right
    } yield (e._1, r._2)

  /** Helper function for [[parseExpression]], [[parseSimpleExpression]], and [[parseTerm]] */
  private def parseExprAux(parse: Parser[Expr], regex: String): Parser[Expr] =
    choice(
      tokens => for {
        l  <- parse(tokens).right
        op <- parseRegex(regex)(l._2).right
        r  <- parse(op._2).right
      } yield (Op(op._1, l._1, r._1), r._2),
      parse)

  /** Parser which parses a [[BoolLit]] */
  private def parseBoolLit: Parser[BoolLit] =
    tokens => for {
      r <- parseRegex("false|true")(tokens).right
    } yield (BoolLit(r._1), r._2)

  /** Parser which parses an [[Num]] */
  private def parseNum: Parser[Num] =
    tokens => for {
      r <- parseRegex("[1-9][0-9]*|0")(tokens).right
    } yield (Num(r._1), r._2)

  /** Parser which parses an [[Ident]] */
  private def parseIdent: Parser[Ident] =
    tokens => for {
      r <- parseRegex("[A-Z][A-Z0-9]*")(tokens).right
    } yield (Ident(r._1), r._2)

  /** Makes a [[Parser]] capable of parsing a regular expression
    *
    * @param regex The regular expression
    * @return A new [[Parser]]
    */
  private def parseRegex[A](regex: String): Parser[String] =
    tokens => tokens match {
      case Seq()                                 => Left(EOFError(Set(regex)))
      case Seq(t, _*) if !t.value.matches(regex) => Left(BadMatchError(Set(regex), t))
      case Seq(t, rest @ _*)                     => Right((t.value, rest.toIndexedSeq))
    }

  /** Tries a series of parsers in order, returning the results of the first success
    *
    * @param parsers The parsers
    * @return A new [[Parser]]
    */
  private def choice[A](parsers: Parser[A]*): Parser[A] =
    tokens => {
      def aux(ps: Seq[Parser[A]], expected: Set[String]): Either[ParseError, (A, TokenStream)] = {
        ps match {
          case Seq() if tokens.isEmpty => Left(EOFError(expected))
          case Seq()                   => Left(BadMatchError(expected, tokens.head))
          case Seq(p, rest @ _*)       => p(tokens) match {
            case Left(e) => aux(rest, expected ++ e.expected)
            case x       => x
          }
        }
      }
      aux(parsers, Set())
    }

  /** Applies the given parser 0 or more times, returning a list of the results
    *
    * @param parse The parser to apply
    * @return A new [[Parser]]
    */
  private def many[A](parse: Parser[A]): Parser[Seq[A]] =
    tokens => {
      def aux(ts: TokenStream, xs: Vector[A]): Either[ParseError, (Seq[A], TokenStream)] = {
        ts match {
          case Seq() => Right(xs, Vector())
          case _     => parse(ts) match {
            case res @ Left(e)  => Right(xs, ts)
            case Right((x, ts)) => aux(ts, xs :+ x)
          }
        }
      }
      aux(tokens, Vector())
    }

}
