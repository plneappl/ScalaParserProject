import org.scalatest._
import SimpleGrammar._
import sext._

class SimpleGrammarSpec extends FlatSpec {
  it should "transform a left-recursive grammar, so that the recursive-descent parser can use it to parse without StackOverflows" in {
    println("before: ")
    println(lrG.treeString)
    println()
    println("after: ")
    val nonLrG = eliminateLR(lrG)
    println(nonLrG.treeString)
    println()
    println()
    println(parseAE("5*2*2+523+22+342*23*2+4*4").treeString)
    println(parse(nonLrG)("5*2*2+523+22+342*23*2+4*4").treeString)
    assert(parseAE("5*2*2+523+22+342*23*2+4*4") == parse(nonLrG)("5*2*2+523+22+342*23*2+4*4"))
    println(transformAE(parseAE("2+3-4")).treeString)
  }


}
