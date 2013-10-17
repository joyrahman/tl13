package edu.utsa.tl13

import edu.utsa.tl13
import Parse._
import TypeCheck._

/** Contains code related to the DOT language */
object DOT {

  private val templateStr = "digraph %s {\n" +
                            "  ordering=out;\n" +
                            "  node [shape = box, style = filled];\n" +
                            "%s" +
                            "}"
  private val descStr     = "  n%d [label=\"%s\",fillcolor=\"%s\",shape=box]\n"
  private val linkStr     = "  n%d -> n%d\n"

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
      case Ident(v)        => v
      case Op(v,_,_)       => v
      case Decl(v,_)       => v
      case Num(v)          => v
    }

  /** Creates DOT notation for a [[Parse.Program]]
    *
    * @param graphName Title for the graph
    * @param program The program
    * @param typeMap The program's type map
    * @param okMap The program's TypeOkMap
    * @return A DOT notation string
    */
  def dotify(graphName: String, program: Program, typeMap: TypeMap, okMap: TypeOkMap): String = {
    val linkMap = mkLinkMap(program)
    val body = linkMap.foldLeft("") {
        (s,kv) => kv match {
          case (p,cs) => {
            val color = okMap(p) match {
                case false => "/pastel13/1"
                case true  => typeMap(p) match {
                  case TL13Int()  => "/pastel13/3"
                  case TL13Bool() => "/pastel13/2"
                  case _          => "/x11/lightgrey"
                }
              }
            cs.foldLeft(s + descStr.format(p.hashCode,label(p),color)) {
              (s,c) => s + linkStr.format(p.hashCode,c.hashCode)
            }
          }
        }
      }
    templateStr.format(graphName.replaceAll("-","_"), body).replaceAll("n-", "n_")
  }

}
