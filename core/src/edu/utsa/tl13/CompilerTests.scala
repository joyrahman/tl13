package edu.utsa.tl13

import edu.utsa.tl13._
import UnitTest._
import ParserTests._
import ScannerTests._

/** All compiler unit tests */
object CompilerTests extends App {

  val tests =
    TestGroup("Compiler",
              ParserTests.tests,
              ScannerTests.tests)

  simpleReport(tests.run)

}
