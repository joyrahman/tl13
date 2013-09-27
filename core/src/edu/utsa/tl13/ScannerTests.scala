package edu.utsa.tl13

import edu.utsa.tl13._
import Scanner._
import UnitTest._

/** [[Scanner]] unit tests */
object ScannerTests {

  private val source =
"""% sqrt.tl13
% This is a program that approximates a sqrt to the smallest
% integer less than the square root by trying each possible sqrt
% until it finds one that is too large

program
  var N as int ;
  var SQRT as int ;
begin
  N := readInt ;
  SQRT := 0 ;

  % go until SQRT exceeds the square root of N
  while SQRT * SQRT <= N do
    SQRT := SQRT + 1
  end ;

  SQRT := SQRT - 1 ; % subtract one SQRT is <= sqrt(N)

  writeInt SQRT ;

end"""

  /** [[Scanner]] unit tests */
  val tests =
    TestGroup("Scanner",
              Test("good",
                   () => assertEqual(Scanner.tokenize(source),
                                     List(Token("program",6,0), Token("var",7,2), Token("N",7,6),
                                          Token("as",7,8), Token("int",7,11), Token(";",7,15),
                                          Token("var",8,2), Token("SQRT",8,6), Token("as",8,11),
                                          Token("int",8,14), Token(";",8,18), Token("begin",9,0),
                                          Token("N",10,2), Token(":=",10,4), Token("readInt",10,7),
                                          Token(";",10,15), Token("SQRT",11,2), Token(":=",11,7),
                                          Token("0",11,10), Token(";",11,12), Token("while",14,2),
                                          Token("SQRT",14,8), Token("*",14,13), Token("SQRT",14,15),
                                          Token("<=",14,20), Token("N",14,23), Token("do",14,25),
                                          Token("SQRT",15,4), Token(":=",15,9), Token("SQRT",15,12),
                                          Token("+",15,17), Token("1",15,19), Token("end",16,2),
                                          Token(";",16,6), Token("SQRT",18,2), Token(":=",18,7),
                                          Token("SQRT",18,10), Token("-",18,15), Token("1",18,17),
                                          Token(";",18,19), Token("writeInt",20,2), Token("SQRT",20,11),
                                          Token(";",20,16), Token("end",22,0)))))

}
