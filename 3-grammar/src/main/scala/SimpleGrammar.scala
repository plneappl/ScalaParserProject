/** Exercise 3.3: A more sophisticated interpreter of grammars
  *
  * In 3.2, you implemented the method `simplifyAE` so that
  * the result of parsing the grammar `ae` is easier to use.
  * We will now make the simplicification work for all grammars.
  *
  * Tasks:
  *
  * 1. Design a data structure for grammars and implement
  *    a grammar interpreter so that users need not write
  *    a simplifying method like `simplifyAE` for every grammar
  *    they define. Instead, the parser of the grammar should
  *    always produce simplified syntax trees.
  *
  * 2. Create a grammar object. You may choose to either write
  *    something equivalent to `NaiveGrammar.ae` in 3.2, or
  *    describe arithmetic expressions with arbitrary spacing
  *    between words (ex. 2.2).
  *
  * 3. Test that your grammar interpreter works as expected.
  *
  *
  * ===================== SPOILER BEGINS =====================
  * You may want to have grammar objects contain information
  * about how to simplify their syntax trees.
  *
  * 1. Instead of `Terminal`, keywords could have their own case
  *    class (say, `Comment`), and the grammar interpreter could
  *    discard all syntax tree nodes created from them.
  *
  * 2. Instead of `Choice`, exp could be defined in terms of a
  *    new case class (say, `Select`). The grammar interpreter
  *    never creates new syntax tree nodes from `Select`.
  * ====================== SPOILER ENDS ======================
  */



object SimpleGrammar extends util.Combinators {
  sealed trait Tree
  case class Leaf(symbol: Symbol, code: String) extends Tree
  case class Branch(symbol: Symbol, children: List[Tree]) extends Tree

  sealed trait RuleRHS {
    def | (rhs: RuleRHS) = Choice(this, rhs)
    def || (rhs: RuleRHS) = Select(this, rhs)
    def ~ (rhs: RuleRHS) = Sequence(this, rhs)
  }

  case class Nonterminal(symbol: Symbol) extends RuleRHS
  case class Terminal(parse: Parser[Tree]) extends RuleRHS
  case class Choice(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS
  case class Sequence(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS

  case class Comment(parse: Parser[Tree]) extends RuleRHS
  case class Select(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS

  val exp       = Nonterminal('exp)
  val add       = Nonterminal('add)
  val mul       = Nonterminal('mul)

  val num       = Terminal(digitsParser('num))
  val sumOf     = Comment(commentParser("sum of "))
  val productOf = Comment(commentParser("product of "))
  val and       = Comment(commentParser(" and "))

  def digitsParser(symbol: Symbol): Parser[Tree] =
    parseRegex("[0-9]+") ^^ { x => Leaf(symbol, x) }

  def keywordParser(keyword: String): Parser[Tree] =
    parseString(keyword) ^^ { x => Leaf('keyword, keyword) }

  def commentParser(keyword: String): Parser[Tree] =
    parseString(keyword) ^^ { x => Leaf('comment, keyword) }

  case class Grammar(start: Nonterminal, rules: Map[Nonterminal, RuleRHS]) {
    def lookup(nonterminal: Nonterminal): RuleRHS = rules(nonterminal)
  }

  val ae: Grammar =
    Grammar(
      start = exp,
      rules = Map(
        exp -> (add || mul || num),
        add -> (sumOf ~ exp ~ and ~ exp),
        mul -> (productOf ~ exp ~ and ~ exp)
      )
    )

  /** Parsing the grammar of your choice.
    * Always produce simplified syntax trees.
    * Should not be hard-coded for arithmetic expressions.
    */

  def parseAE(input: String): Tree = simplifyAE({
    var parser = parseNonterminal(ae.start, ae)
    parser(input) match {
      case Some((t, rest)) => t
      case None => sys.error("Not an Expression: " + input)

    }
  })

  def parseNonterminal(nonterminal: Nonterminal, grammar: Grammar): Parser[Tree] =
    parseRHS(grammar lookup nonterminal, grammar) ^^ {
      children => Branch(nonterminal.symbol, children)
    }

  def parseRHS(ruleRHS: RuleRHS, grammar: Grammar): Parser[List[Tree]] = 
    ruleRHS match {
      case nt:Nonterminal => parseNonterminal(nt, grammar) ^^ { 
        t => {
          println("t: ")
          println(t)
          println()
          List(t)
        }
      }

      case Comment(parser) => (parser ^^ {t => List.empty })
      case Terminal(parser) => (parser ^^ { t => List(t) })      

      case Sequence(s1, s2) => (parseRHS(s1, grammar) ~ parseRHS(s2, grammar)) ^^ {
        case (t1:List[Tree], t2:List[Tree]) => t1 ::: t2
      }
    
      case Select(c1, c2) => (parseRHS(c1, grammar) | parseRHS(c2, grammar)) 

      case Choice(c1, c2) => parseRHS(c1, grammar) | parseRHS(c2, grammar)
    }

  def simplifyAE(syntaxTree: Tree): Tree = syntaxTree match {
    case Branch(symbol, list) => {
      var rule = ae.lookup(Nonterminal(symbol))
      rule match {
        case Select(_, _) => simplifyAE(list.head)
        case t => Branch(symbol, list.map(simplifyAE))
      }
      
    }
    case t => t
    
  }
}
