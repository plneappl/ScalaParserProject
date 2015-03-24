import org.scalatest._

import SimpleGrammar._

import sext._

class SimpleGrammarSpec extends FlatSpec {
  it should "parse and simplify arithmetic expressions" in {
    println(epsilonParser("asd"))

    println(parseAE("5 / 2"          ).treeString + "\n")
    println(parseAE("3 + 5"          ).treeString + "\n")
    println(parseAE("3 + 5 / 2"          ).treeString + "\n")
    println(parseAE("3 - 2 + 4 - 2"      ).treeString + "\n")
    println(parseAE("1256 + 25 * 48 / 9" ).treeString + "\n")

    println(parseAE("2 + 2 == 4"            ).treeString + "\n")
    println(parseAE("25 * 8 == 500 / 2 - 50").treeString + "\n")

    println(parseAE("if 1 == 1 then 2 else 3").treeString + "\n")
    println(parseAE("if 2 + 2 == 5 then 1900 + 84 else 5 * 403").treeString + "\n")

    assert(parseAE("1234") == Leaf('num, "1234"))

    assert(parseAE("1 + 2") ==
      Branch('add, List(
        Leaf('num, "1"),
        Leaf('num, "2"))))

  }

  it should "evaluate arithmetic expressions" in {
    assert( eval(parseAE("1234")) == 1234 )

    assert( eval(parseAE("1 + 2")) == 3 )

    assert( eval(parseAE("2*1234")) == 2468 )

    //this now works
    assert( eval(parseAE("2*3+4")) == 10 )

    println(transformAE(parseAE("5 - 2 - 1")).treeString)
    println(parseAE("32 / 4 / 2").treeString)

    assert(eval(transformAE(parseAE("5 - 2 - 1")))  == 2)
    assert(eval(transformAE(parseAE("36 / 6 / 2"))) == 3)

    println(eval(transformAE(parseAE("8/2/2-4/2/1"))))

    println(parseAndEval("2 + 2 == 4"))
    println(parseAndEval("25 * 8 == 500 / 2 - 50"))

    println(parseAndEval("if 1 == 1 then 2 else 3"))
    println(parseAndEval("if 2 + 2 == 5 then 1900 + 84 else 5 * 403"))

  }

  it should "unparse trees" in {
    val orig = "if 1 + 1 == 2 then if 2 + 2 == 5 then 1111 + 222 + 33 + 4 else 4444 * 333 * 22 * 1 else if 1 == 2 then 2 + 2 else 4 * 5"
    val tree = parseAE(orig)
    println("Original: " + orig)
    println("Unparsed: " + unparse(tree))
    println(pretty(tree, 20))
  }

}
