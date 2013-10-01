package edu.utsa.tl13

import edu.utsa.tl13.Parser._

/** Contains code related to the DOT language */
object DOT {

  private val headerStr = "digraph %s {\n" +
                          "  ordering=out;\n" +
                          "  node [shape = box, style = filled];\n"

  private val descStr = "  n%d [label=\"%s\",fillcolor=\"/x11/white\",shape=box]\n"
  private val linkStr = "  n%d -> n%d\n"

  private case class DotifyState(res: String, links: Map[Int,Int])

  /** Creates a DOT graph from a parsed AST
    *
    * @param graphName Name of the graph
    * @param node The AST
    * @return The DOT graph
    */
  def dotify(graphName: String, node: Node): String = {
    (node.fold(DotifyState(headerStr.format(graphName), Map())) {
      (state, node) =>
        val newLinks = node.children
                         .foldLeft(state.links)
                           { (ls,n) => ls + ((n.hashCode) -> node.hashCode) }
        var newRes = state.res + descStr.format(node.hashCode, node.value) +
          (newLinks.contains(node.hashCode) match {
            case true  => linkStr.format(state.links(node.hashCode), node.hashCode)
            case false => ""
          })
        state.copy(newRes, newLinks)
    }.res + "}").replaceAll("n-", "n_")
  }

}
