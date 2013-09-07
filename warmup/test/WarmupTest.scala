import org.scalatest.FunSuite
import Expr._

class WarmupTest extends FunSuite {
  test("Times duplicate 1 * 3, 3 * 1") {
    assert( Times(Num(1), Num(3)).duplicate.print === "(* 1 3)" )
    assert( Times(Num(3), Num(1)).duplicate.print === "(* 3 1)" )
  }

  test("Times duplicate 1 * 2") {
    assert( Times(Num(1), Num(2)).duplicate.print === "(+ 1 1)" )
  }

  test("Times duplicate 2 * 1") {
    assert( Times(Num(2), Num(1)).duplicate.print === "(+ 1 1)" )
  }

  test("Times duplicate nested") {
    val expr: Expr = Times(Num(2), Plus(Num(1),Times(Num(3),Num(2))))
    assert( expr.duplicate.print === "(+ (+ 1 (+ 3 3)) (+ 1 (+ 3 3)))" )
  }
}
