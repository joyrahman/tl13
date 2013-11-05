package edu.utsa.tl13

import edu.utsa.tl13
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
    def dotifyAST(program: Program, graphName: String, typeMap: TypeMap, okMap: TypeOkMap): String = {
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
}
