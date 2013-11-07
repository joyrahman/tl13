package edu.utsa.tl13

import edu.utsa.tl13
import ILOC._
import Parse._
import TypeCheck._

/** Contains code related to the DOT language */
object DOT {

  object AST {

    /** Represents a DOT graph
      *
      * @param name The name of the graph
      * @param items The attributes and relationships in the graph
      */
    case class Graph(val name: String, val items: Item*) {
      override def toString = ("digraph %s {\n"                          +
                                 "  ordering=out;\n"                       +
                                 "  node [shape = box, style = filled];\n" +
                                 "%s"                                      +
                                 "}").format(name, items.mkString)
    }

    /** Represents either a relationship or an attribute in a DOT graph */
    sealed abstract class Item

    /** Represents an attribute in a DOT graph
      *
      * @param node The name of the node
      * @param label The node's label
      * @param color The node color
      */
    case class Attribute(node: String, label: String, color: String) extends Item {
      override def toString =
        "  %s [label=\"%s\",fillcolor=\"%s\",shape=box]\n".format(node, label, color)
    }

    /** Represents a relationship in a DOT graph
      *
      * @param nodes The nodes in the graph
      */
    case class Relationship(nodes: String*) extends Item {
      override def toString = "  %s\n".format(nodes.mkString(" -> "))
    }

    /** Creates a map of [[Parse.Node]]'s to their children */
    private def mkLinkMap(node: Node): Map[Node,Seq[Node]] =
      node.prewalk(Map[Node,Seq[Node]]()) { (m,n) => m + (n -> n.children) }

    /** Gets a label for a DOT graph node */
    private def label(node: Node): String = node match {
        case _: ReadInt      => ":= readInt"
        case _: AsgnExpr     => ":="
        case _: If           => "if"
        case _: While        => "while"
        case _: WriteInt     => "writeInt"
        case _: Program      => "program"
        case _: TL13Int      => "int"
        case _: TL13Bool     => "bool"
        case _: TL13None     => "none"
        case _: StatementSeq => "stmt list"
        case _: Decls        => "decl list"
        case BoolLit(v)      => v
        case Ident(v)        => v
        case Op(v,_,_)       => v
        case Decl(v,_)       => v
        case Num(v)          => v
      }

    /** Gets a string representation of a [[Parse.Node]] suitable for use in a DOT graph */
    private def mkDOTNode(node: Node): String = ("n" + node.hashCode).replaceAll("n-", "n_")

    /** Creates DOT notation for a [[Parse.Program]]
      *
      * @param program The program
      * @param graphName Title for the graph
      * @param typeMap The program's type map
      * @param okMap The program's TypeOkMap
      * @return A DOT notation string
      */
    def dotify(program: Program, graphName: String, typeMap: TypeMap, okMap: TypeOkMap): String = {
      val linkMap = mkLinkMap(program)
      val items = linkMap.foldLeft(Vector[Item]()) {
          (is,kv) => kv match {
            case (p, cs) => {
              val color = okMap(p) match {
                  case false => "/pastel13/1"
                  case true  => typeMap(p) match {
                    case TL13Int()  => "/pastel13/3"
                    case TL13Bool() => "/pastel13/2"
                    case _          => "/x11/lightgrey"
                  }
                }
              cs.foldLeft(is :+ Attribute(mkDOTNode(p), label(p), color)) {
                (is,c) => is :+ Relationship(mkDOTNode(p), mkDOTNode(c))
              }
            }
          }
        }
      Graph(graphName.replaceAll("-", "_"), items:_*).toString
    }

  }

  object ILOC {

    /** Represents a DOT graph
      *
      * @param name The name of the graph
      * @param items The attributes and relationships in the graph
      */
    case class Graph(val name: String, val entry: String, val exit: String, val items: Item*) {
      override def toString = ("digraph %s {\n"             +
                               "  node [shape = none];\n"   +
                               "  edge [tailport = s];\n"   +
                               "  entry\n"                  +
                               "  subgraph cluster {\n"     +
                               "    color=\"/x11/white\"\n" +
                               "%s"                         +
                               "  }\n"                      +
                               "  entry -> %s\n"            +
                               "  %s -> exit\n"             +
                               "}").format(name, items.mkString, entry, exit)
    }

    /** Represents either a relationship or an attribute in a DOT graph */
    sealed abstract class Item

    /** Represents an attribute in a DOT graph
      *
      * @param node The name of the node
      * @param label The node's label
      */
    case class Attribute(node: String, label: String) extends Item {
      override def toString =
        "    %s [label=%s,fillcolor=\"/x11/white\",shape=box]\n".format(node, label)
    }

    /** Represents a relationship in a DOT graph
      *
      * @param nodes The nodes in the graph
      */
    case class Relationship(nodes: String*) extends Item {
      override def toString = "    %s\n".format(nodes.mkString(" -> "))
    }

    /** Gets a label for a DOT graph node */
    private def label(block: Block): String = {
      val s = "<<table border=\"0\"><tr><td border=\"1\">B%s</td></tr>%s</table>>"
      val before = "<tr><td align=\"left\">"
      val after = "</td></tr>"
      s.format(block.label,
               block.instrs.map(instrToString(_))
                 .mkString(before, after+before, after))
    }

    /** Gets a string representation of a [[Parse.Node]] suitable for use in a DOT graph */
    private def mkDOTNode(block: Block): String = ("n" + block.hashCode).replaceAll("n-", "n_")

    private def instrToString(instr: Instruction): String =
      instr match {
        case loadI(c1, r2)      => "loadI %s =&gt; R%s".format(c1, r2)
        case add(r1, r2, r3)    => "add R%s, R%s =&gt; R%s".format(r1, r2, r3)
        case sub(r1, r2, r3)    => "sub R%s, R%s =&gt; R%s".format(r1, r2, r3)
        case mult(r1, r2, r3)   => "mult R%s, R%s =&gt; R%s".format(r1, r2, r3)
        case div(r1, r2, r3)    => "div R%s, R%s =&gt; R%s".format(r1, r2, r3)
        case mod(r1, r2, r3)    => "mod R%s, R%s =&gt; R%s".format(r1, r2, r3)
        case cmp_EQ(r1, r2, r3) => "cmp_EQ R%s, R%s =&gt; R%s".format(r1, r2, r3)
        case cmp_NE(r1, r2, r3) => "cmp_NE R%s, R%s =&gt; R%s".format(r1, r2, r3)
        case cmp_LE(r1, r2, r3) => "cmp_LE R%s, R%s =&gt; R%s".format(r1, r2, r3)
        case cmp_GE(r1, r2, r3) => "cmp_GE R%s, R%s =&gt; R%s".format(r1, r2, r3)
        case cmp_LT(r1, r2, r3) => "cmp_LT R%s, R%s =&gt; R%s".format(r1, r2, r3)
        case cmp_GT(r1, r2, r3) => "cmp_GT R%s, R%s =&gt; R%s".format(r1, r2, r3)
        case writeInt(r1)       => "writeInt R%s".format(r1)
        case readInt(r1)        => "readInt =&gt; R%s".format(r1)
        case i2i(r1, r2)        => "i2i R%s =&gt; R%s".format(r1, r2)
        case cbr(r1, l2, l3)    => "cbr R%s -&gt; B%s, B%s".format(r1, l2, l3)
        case jumpI(l1)          => "jumpI -&gt; B%s".format(l1)
        case exit()             => "exit"
      }

    /** Creates DOT notation for a [[Parse.Program]]
      *
      * @param program The program
      * @param graphName Title for the graph
      * @return A DOT notation string
      */
    def dotify(blocks: Iterable[Block], graphName: String): String = {
      val linkMap = mkBlockLinkMap(blocks)
      val items = linkMap.foldLeft(Vector[Item]()) {
          (is,kv) => kv match {
            case (p, cs) => {
              cs.foldLeft(is :+ Attribute(mkDOTNode(p), label(p))) {
                (is,c) => is :+ Relationship(mkDOTNode(p), mkDOTNode(c))
              }
            }
          }
        }
      Graph(graphName.replaceAll("-", "_"), mkDOTNode(firstBlock(blocks)),
            mkDOTNode(lastBlock(blocks)), items:_*).toString
    }

  }

}
