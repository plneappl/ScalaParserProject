/** 
  Exercise 4
  5.:
      This fails, since it tries to expand as follows:
        exp -> add -> exp + exp -> add + exp -> ... 
  6.: 
      It's called left recursion...
  8.:
      Operator precedence is missing. Instead it does precedence from right to left.
**/
import sext._


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
  val expPar    = Nonterminal('expPar)
  val expPrec   = Nonterminal('expPrec)
  val addP      = Nonterminal('addP)
  val mulP      = Nonterminal('mulP)

  val num       = Terminal(digitsParser('num))
  val #+        = Comment(commentParser("+"))
  val #*        = Comment(commentParser("*"))
  val lp        = Comment(commentParser("("))
  val rp        = Comment(commentParser(")"))

  def digitsParser(symbol: Symbol): Parser[Tree] =
    parseRegex("[0-9]+") ^^ { x => Leaf(symbol, x) }

  def keywordParser(keyword: String): Parser[Tree] =
    parseRegex("\\s*\\Q" + keyword + "\\E\\s*") ^^ { x => Leaf('keyword, keyword) }

  def commentParser(keyword: String): Parser[Tree] =
    parseRegex("\\s*\\Q" + keyword + "\\E\\s*") ^^ { x => Leaf('comment, keyword) }

  case class Grammar(start: Nonterminal, rules: Map[Nonterminal, RuleRHS]) {
    def lookup(nonterminal: Nonterminal): RuleRHS = rules(nonterminal)
  }

  //do precedence by first looking for the adds adding the muls, then the muls mul'ing the nums
  val ae: Grammar =
    Grammar(
      start = exp,
      rules = Map(
        exp -> (expPrec || expPar),
        expPar -> (addP || mulP || num),
        addP -> ( lp ~ num ~ #+ ~ exp ~ rp ),
        mulP -> ( lp ~ num ~ #* ~ exp ~ rp ), 

        expPrec -> (add || num),
        add -> ((mul ~ #+ ~ add) || mul),
        mul -> ((expPar ~ #* ~ mul) || num || expPar)
      )
    )

  /*
  val ae: Grammar =
    Grammar(
      start = exp,
      rules = Map(
        exp -> (add || mul || num),
        add -> (exp ~ #+ ~ exp),
        mul -> (exp ~ #* ~ exp)
      )
    )
    */

  /** Parsing the grammar of your choice.
    * Always produce simplified syntax trees.
    * Should not be hard-coded for arithmetic expressions.
    */

  def parseAE(input: String): Tree = simplifyAE({
    var parser = parseNonterminal(ae.start, ae)
    parser(input) match {
      case Some((t, rest)) => {
        if(rest.isEmpty)
          t
        else
          sys.error("Rest not empty: " + rest)
      }
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
          List(t)
        }
      }

      case Comment(parser) => (parser ^^ {t => {
        //println(t)
        List.empty 
      }})

      case Terminal(parser) => (parser ^^ { t => {
        //println(t)
        List(t) 
      }})      

      case Sequence(s1, s2) => (parseRHS(s1, grammar) ~ parseRHS(s2, grammar)) ^^ {
        case (t1:List[Tree], t2:List[Tree]) => {
          //println(t1)
          //println(t2)
          t1 ::: t2
        }
      }
    
      case Select(c1, c2) => (parseRHS(c1, grammar) | parseRHS(c2, grammar)) 

      case Choice(c1, c2) => parseRHS(c1, grammar) | parseRHS(c2, grammar)
    }

  def simplifyAE(syntaxTree: Tree): Tree = syntaxTree match {
    case Branch(symbol, list) => {
      var rule = ae.lookup(Nonterminal(symbol))
      rule match {
        case Select(_, _) => {
          if(list.length == 1)
            simplifyAE(list.head)
          else
            Branch(symbol, list.map(simplifyAE))
        }
        case t => Branch(symbol, list.map(simplifyAE))
      }
      
    }
    case t => t 
  }

  def eval(t: Tree): Int = t match {
      case Branch('add, List(lhs, rhs)) =>
        eval(lhs) + eval(rhs)
      
      case Branch('addP, List(lhs, rhs)) =>
        eval(lhs) + eval(rhs)
  

      case Branch('mul, List(lhs, rhs)) =>
        eval(lhs) * eval(rhs)

      case Branch('mulP, List(lhs, rhs)) =>
        eval(lhs) * eval(rhs)
  
      case Leaf('num, code) =>
        code.toInt
    }


}
