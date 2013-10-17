package edu.utsa.tl13

import edu.utsa.tl13._
import UnitTest._
import ParseTests._
import ScanTests._

/** All compiler unit tests */
object CompilerTests extends App {

  val tests =
    TestGroup("Compiler",
              ParseTests.tests,
              ScanTests.tests)

  simpleReport(tests.run)

}
