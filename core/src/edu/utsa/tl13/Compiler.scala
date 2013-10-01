package edu.utsa.tl13

import Parser._
import DOT._
import Scanner._

object Compiler {

  /** Automatically calls close on a resource after running a function
    *
    * @tparam T The resource which has a close method
    * @tparam A The return value of the given function, also returned from using
    * @param resource The resource to use; passed into the given function.
    * @param The function to run with the given resource
    */
  private def using[T <: { def close() }, A]
    (resource: T)
    (block: T => A)
  : A =
  {
    try {
      block(resource)
    } finally {
      if (resource != null) resource.close()
    }
  }

  def main(args: Array[String]) {
    if (args.length != 1 || !args(0).endsWith(".tl13")) {
      println("You must pass in a file ending with .tl13")
    } else {
      try {
        val baseName = args(0).substring(args(0).lastIndexOf("/") + 1, args(0).length - 5)
        val dotstr =
          using(scala.io.Source.fromFile(args(0))) { r =>
            val ast = parseProgram(tokenize(r.mkString))
            dotify(baseName, ast)
          }
        using(new java.io.FileWriter(baseName + ".ast.dot")) { r =>
          r.write(dotstr, 0, dotstr.length)
        }
      } catch {
        case e: Throwable => println(e)
      }
    }
  }

}
