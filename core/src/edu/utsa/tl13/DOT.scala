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
  def mkLinkMap(node: Node): Map[Node,Seq[Node]] =
    node.prewalk(Map[Node,Seq[Node]]()) { (m,n) => m + (n -> n.children) }

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
            cs.foldLeft(s + descStr.format(p.hashCode,p.value,color)) {
              (s,c) => s + linkStr.format(p.hashCode,c.hashCode)
            }
          }
        }
      }
    templateStr.format(graphName.replaceAll("-","_"), body).replaceAll("n-", "n_")
  }

}
