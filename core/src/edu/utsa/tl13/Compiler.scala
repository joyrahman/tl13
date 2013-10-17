package edu.utsa.tl13

import Parse._
import DOT._
import Scan._
import TypeCheck._

object Compiler {

  /** Automatically calls close on a resource after running a function
    *
    * @tparam T The resource which has a close method
    * @tparam A The return value of the given function, also returned from using
    * @param resource The resource to use; passed into the given function.
    * @param The function to run with the given resource
    */
  private def using[T <: { def close() }, A](resource: T)(block: T => A): A = {
    try {
      block(resource)
    } finally {
      if (resource != null) resource.close()
    }
  }

  /** Reads text from a file
    *
    * @param filename The name of the file
    * @return The file contents
    */
  private def readFile(filename: String) =
    using(scala.io.Source.fromFile(filename)) { r => r.mkString }

  /** Writes text to a file
    *
    * @param filename The file's name
    * @param contents The contents to write
    */
  private def writeFile(filename: String, contents: String) =
    using(new java.io.FileWriter(filename)) { r =>
      r.write(contents, 0, contents.length)
    }

  /** Writes the DOT notation for the program */
  private def writeDotFile(baseName: String, program: Program, typeMap: TypeMap, okMap: TypeOkMap) = {
    val outName = baseName + ".ast.dot"
    println("writing file: " + outName)
    writeFile(outName, dotifyAST(program, baseName, typeMap, okMap))
  }

  def main(args: Array[String]) {
    if (args.length != 1 || !args(0).endsWith(".tl13")) {
      println("You must pass in a file ending with .tl13")
    } else {
      try {
        val baseName = args(0).substring(args(0).lastIndexOf("/") + 1, args(0).length - 5)
        val src = readFile(args(0))
        val program = parseProgram(tokenize(src))
        program match {
          case Left(BadMatchError(e, t)) =>
            println("line %d, column %d: expecting %s, got %s"
                      .format(t.line, t.column, e.mkString(", "), t.value))
          case Left(EOFError(e))         =>
            println("expecting %s, got unexpected end of file".format(e.mkString(", ")))
            None
          case Right(a)                  =>
            typeCheck(a._1) match {
              case Right((typeMap,okMap)) => {
                writeDotFile(baseName, program.right.get._1, typeMap, okMap)
                // TODO output ILOC, MIPS, etc
              }
              case Left((typeMap,okMap)) => writeDotFile(baseName, program.right.get._1, typeMap, okMap)
            }
        }
      } catch {
        case e: Throwable => println(e)
      }
    }
  }

}
