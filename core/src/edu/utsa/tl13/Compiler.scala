package edu.utsa.tl13

import Parse._
import DOT._
import ILOC._
import MIPS._
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

  /** Checks parse success, reports it and exits */
  private def handleProgramParse(program: Either[ParseError, (Parse.Program,TokenStream)]) {
    program match {
      case Left(BadMatchError(e, t)) => {
        Console.err.println("PARSER ERROR: line %d, column %d: expecting %s, got %s"
                              .format(t.line, t.column, e.mkString(", "), t.value))
        System.exit(-1)
      }
      case Left(EOFError(e))         => {
        Console.err.println("PARSER ERROR: expecting %s, got unexpected end of file"
                              .format(e.mkString(", ")))
        System.exit(-1)
      }
      case _                         => Unit
    }
  }

  /** Writes the AST dot file, detects and reports type errors */
  private def handleTypeCheck(program: Parse.Program, baseName: String, typeMap :TypeMap, okMap: TypeOkMap) {
    val dotFileName = baseName + ".ast.dot"
    println("writing file: " + dotFileName)
    writeFile(dotFileName, AST.dotify(program, baseName, typeMap, okMap))
    if ( !isWellTyped(okMap) ) {
      Console.err.println("TYPE ERROR DETECTED")
      System.exit(-2)
    }
  }

  /** Writes the ILOC control flow graph */
  private def handleILOC(program: Parse.Program, baseName: String) = {
    val blocks = ilocifyProgram(program)
    val dotFileName = baseName + ".cfg.dot"
    println("writing file: " + dotFileName)
    writeFile(dotFileName, DOT.ILOC.dotify(blocks, baseName))
    blocks.toSeq
  }

  /** Writes out the MIPS file */
  private def handleMIPS(ilocBlocks: Seq[ILOC.Block], baseName: String) {
    val mipsProgram = mipsifyProgram(ilocBlocks)
    val sFileName = baseName + ".s"
    println("writing file: " + sFileName)
    writeFile(sFileName, mipsProgram.toString)
  }

  def main(args: Array[String]) {
    if (args.length != 1 || !args(0).endsWith(".tl13")) {
      println("You must pass in a file ending with .tl13")
    } else {
      try {
        val baseName = args(0).substring(args(0).lastIndexOf("/") + 1, args(0).length - 5)
        val src = readFile(args(0))

        val program = parseProgram(tokenize(src))
        handleProgramParse(program)

        val (typeMap, typeOkMap) = typeCheck(program.right.get._1)
        handleTypeCheck(program.right.get._1, baseName, typeMap, typeOkMap)

        val ilocBlocks = handleILOC(program.right.get._1, baseName)

        handleMIPS(ilocBlocks, baseName)
      } catch {
        case e: Throwable => println(e)
      }
    }
  }

}
