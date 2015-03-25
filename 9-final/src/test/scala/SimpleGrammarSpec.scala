import org.scalatest._

import SimpleGrammar._

import sext._

class SimpleGrammarSpec extends FlatSpec {
  it should "" in {
    println("before: ")
    println(lrG.treeString)
    println()
    println("after: ")
    val nonLrG = eliminateLR(lrG)
    println(nonLrG.treeString)
    println()
    println()
    println(parseAE("5*2*2+523+22+342*23*2+4*4").treeString)
    println(simplify(nonLrG)(parse(nonLrG)("5*2*2+523+22+342*23*2+4*4")).treeString)
  }


}
