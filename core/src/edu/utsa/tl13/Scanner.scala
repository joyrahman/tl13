package edu.utsa.tl13

/** Module for scanner-related functionality */
object Scanner {

  /** Represents a source file token
    *
    * @constructor Creates a new token with token string, line number and column number
    * @param value the token's string value
    * @param line the token's line number (1 based)
    * @param column the token's column (0 based
    */
  case class Token(value: String, line: Int, column: Int)

  /** Breaks a source string into Token objects
    *
    * @param input the source
    * @return a traversable set of Token objects
    */
  def tokenize(input: String): Traversable[Token] = {
    def tokenizeLine(linePair: Pair[String,Int]): Traversable[Token] = {
      val (line, lineNum) = linePair
      val tokens = "\\S+|\\s+".r.findAllIn(line).foldLeft(Vector[Token]()) {
        (v:Vector[Token], s:String) => {
          val col: Int = if ( v.isEmpty ) 0 else v.last.column + v.last.value.length
          v :+ Token(s, lineNum + 1, col)
        }
      }
      tokens.filter { _.value.matches("\\S+") }
    }

    val lines = input.replaceAll("%.*?\n", "\n").split("\\r?\\n").zipWithIndex
    lines.flatMap(tokenizeLine)
  }

}
