/** Tests for Exercise 2.2
  *
  * Uncomment `CombinatorsSpec` after Exercise 2.1 is done.
  *
  * Beware: This file contains occurrences of ???. If those are
  * not replaced, then tests will fail.
  */

import org.scalatest._

/* UNCOMMENT AFTER COMPLETION OF 2.1 */

class CombinatorsSpec extends FlatSpec with Combinators {

  /* Recall that by writing combinators taking functions as
   * arguments, we avoid code duplication in parsers. We can
   * avoid code duplication in tests in the same way. This is a
   * method that checks whether any given parser agrees with the
   * first parser of Exercise 2.1.
   */
  def testExp(parseExp: Parser[Exp]): Unit = {
    val e1code = "sum of 6 and product of 6 and 6"
    val e2code = "product of 6 and sum of 4 and 3"
    assert(parseExp(e1code) == Some((e1, "")))
    assert(parseExp(e2code) == Some((e2, "")))

    // Your tests here
  }

  "The `choice` combinator" should "work for `parseExp3`" in testExp(parseExp3)

  /* Check whether any given parser agrees with the first parser
   * of products in Exercise 2.1.
   */
  def testMul(parseMul: Parser[Exp]): Unit = {
    val e1code = "product of 6 and 2"
    val e2code = "product of 5 and product of 2 and 4"
    assert(parseMul(e1code) == Some((Mul(Num(6), Num(2)), "")))
    assert(parseMul(e2code) == Some((Mul(Num(5), Mul(Num(2), Num(4))), "")))
  }

  "The `sequence` combinator" should "work for `parseMul3`" in testMul(parseMul3)

  "The `postprocess` combinator" should "work for `parseMul4`" in testMul(parseMul4)

  "One" should "be able to implement the AE parser with combinators" in testExp(exp)

  "The `zeroOrMore` combinator" should "build parsers of lists" in {
    val nums = zeroOrMore(exp)

    assert(nums("abcdefg") == Some((List.empty, "abcdefg")))

    assert(nums("1234 abcd") == Some((List(Num(1234)), " abcd")))

    assert(nums("sum of 1 and 1sum of 2 and 2sum of 3 and 3rd power of 2") ==
      Some(
        ( List(
            Add(Num(1), Num(1)),
            Add(Num(2), Num(2)),
            Add(Num(3), Num(3))
          ),
          "rd power of 2" )
      )
    )
  }

  /* Task 2.2.4: Replace `pending` by a real test for `oneOrMore`. */
  def testOneOrMore(): Unit = {
    val e1code = "   a b"
    val e2code = "a b"
    assert(oneOrMore(parseSpace)(e1code) == Some((List(" ", " ", " "), "a b")))
    assert(parseMin0Spaces(e2code) == Some(("", "a b")))
    assert(parseMin1Spaces(e2code) == None)

    assert(parseMin0Spaces(e1code) == Some(("   ", "a b")))
    assert(parseMin1Spaces(e1code) == Some(("   ", "a b")))
  }

  "The `oneOrMore` combinator" should "build parsers of nonempty lists" in testOneOrMore()

  /* Task 2.2.5: Replace `pending` by a real test for `parse3`. */
  def testParse3(): Unit = {
    assert(sumOf3("sum of") == Some(("of", "")))
    assert((exp3 <~ parseMin1Spaces)("5  ") == Some((Num(5), "")))
    assert((parseMin0Spaces ~> sumOf3 ~> parseMin1Spaces ~> exp3)("sum of 5 and") == Some(Num(5), " and"))
    assert((parseMin0Spaces ~> sumOf3 ~> parseMin1Spaces ~> exp3 )("sum of 5 and ") == Some(Num(5), " and "))
    assert(parseAnd(" and ") == Some(" and ", ""))
    assert((parseMin0Spaces ~> add3)(" sum of 1 and 1") == Some(Add(Num(1), Num(1)), ""))
    assert(exp3(" sum    of     1   and 1 ") == Some(Add(Num(1), Num(1)), " "))
    assert(parse3(" sum    of     1   and 1 ") == Add(Num(1), Num(1)))
  }

  "`parse3`" should "parse expressions with spaces between words" in testParse3()
}

/* UNCOMMENT AFTER COMPLETION OF 2.1 */
