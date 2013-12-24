package edu.utsa.tl13

/** Module containing unit test code */
object UnitTest {

  /** Signals some unit test assertion failed
    *
    * @param msg The assertion message
    */
  class AssertionFailedError(msg: String) extends Exception(msg)

  /** Changes console color to green */
  val GREEN = "\u001B[32m"

  /** Changes console color to red */
  val RED = "\u001B[31m"

  /** Resets console color */
  val RESET = "\u001B[0m"

  /** Base class for test types */
  abstract class TestBase {
    /** Runs the test */
    def run: TestResult
  }

  /** The result of a run [[Test]]
    *
    * @param test The [[Test]] that was run
    * @param error The error that occurred, if any
    * @param subResults Further child [[TestResult]]s that were run
    */
  case class TestResult(test: TestBase, error: Option[String], subResults: TestResult*)

  /** Contains other [[Test]]s or [[TestGroup]]s as a heirarchy
    *
    * @param name The name of the group
    * @param subTests child [[TestBase]]s
    */
  case class TestGroup(name: String, subTests: TestBase*) extends TestBase {
    def run: TestResult = TestResult(this, None, (for { t <- subTests } yield t.run):_*)
  }

  /** Represents a test to run
    *
    * @param name The name of the test
    * @param test The test to be run
    */
  case class Test(name: String, test: () => Any) extends TestBase {
    def run: TestResult = {
      val error =
        try {
          test()
          None
        } catch {
          case e: AssertionFailedError => Some(e.getMessage)
          case e: Exception            => Some(e.toString)
        }
      TestResult(this, error)
    }
  }

  /** Asserts that 2 objects are equal to each other */
  def assertEqual(a: Any, b: Any) =
    if (a != b) throw new AssertionFailedError("%s does not equal %s".format(a,b))

  /** Asserts that an exception is thrown
    *
    * @tparam A The exception type that should be thrown
    * @param expr The expression that should throw an exception
    */
  def assertThrows[A <: Throwable](expr: => Any)(implicit m: Manifest[A]) {
    val clazz = m.erasure.asInstanceOf[Class[A]]
    val caught =
      try {
        expr
        None
      } catch {
        case e: Throwable => {
          if (e.getClass == clazz)
            Some(e)
          else
            throw new AssertionFailedError("expected exception %s, got %s".format(clazz, e))
        }
      }
    caught match {
      case Some(e) => Unit
      case None    => throw new AssertionFailedError("expected exception, got nothing")
    }
  }

  /** Prints out the results of a set of run tests
    *
    * @param result The test results
    */
  def simpleReport(result: TestResult) {
    def aux(result: TestResult, parents: Vector[String], total: Pair[Int,Int]): Pair[Int,Int] = {
      result.test match {
        case t: TestGroup => {
          val totals = for { r <- result.subResults } yield aux(r, parents :+ t.name, total)
          totals.reduceLeft { (p1,p2) => (p1._1 + p2._1, p1._2 + p2._2) }
        }
        case t: Test      =>
          if (result.error.isEmpty) {
            (total._1 + 1, total._2)
          } else {
            println("%sFAILURE%s: %s: %s".format(RED, RESET, (parents :+ t.name).mkString(" - "), result.error.get))
            (total._1, total._2 + 1)
          }
      }
    }
    aux(result, Vector[String](), (0,0)) match {
      case (successes, 0)        => println("%s%d successful test(s)%s".format(GREEN, successes, RESET))
      case (0, failures)         => println("S%d failed test(s)%s".format(RED, failures, RESET))
      case (successes, failures) =>
        println("%s%d successful test(s)%s, %s%d failed test(s)%s"
                .format(GREEN, successes, RESET, RED, failures, RESET))
    }
  }

}
