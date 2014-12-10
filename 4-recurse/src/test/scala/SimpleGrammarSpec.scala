import org.scalatest._

import SimpleGrammar._

import sext._

class SimpleGrammarSpec extends FlatSpec {
  it should "parse and simplify arithmetic expressions" in {
    assert(parseAE("1234") == Leaf('num, "1234"))

    assert(parseAE("1 + 2") ==
      Branch('add, List(
        Leaf('num, "1"),
        Leaf('num, "2"))))

    assert(parseAE("1+2*1234") ==
      Branch('add, List(
        Leaf('num,"1"), 
        Branch('mul, List(
          Leaf('num,"2"), Leaf('num,"1234")
          ))
        ))
      )
  }

  it should "evaluate arithmetic expressions" in {
    assert( eval(parseAE("1234")) == 1234 )

    assert( eval(parseAE("1 + 2")) == 3 )

    assert( eval(parseAE("2*1234")) == 2468 )

    //since we eval right to left, this shouldn't be 6+4=10 but 2*7=14
    assert( eval(parseAE("2*3+4")) == 14 )
  }

  // Your tests here
}
