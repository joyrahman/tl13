package edu.utsa.tl13

import edu.utsa.tl13._
import ScannerTests._
import UnitTest._

/** All compiler unit tests */
object CompilerTests extends App {

  val tests =
    TestGroup("Compiler",
              ScannerTests.tests)

  simpleReport(tests.run)

}
