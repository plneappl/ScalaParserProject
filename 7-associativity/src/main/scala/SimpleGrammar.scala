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
    def !~ (rhs: RuleRHS) = LSequence(this, rhs)
  }

  case class Nonterminal(symbol: Symbol) extends RuleRHS
  case class Terminal(parse: Parser[Tree]) extends RuleRHS
  case class Choice(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS
  case class Sequence(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS
  case class LSequence(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS

  case class Comment(parse: Parser[Tree]) extends RuleRHS
  case class Select(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS

  val exp       = Nonterminal('exp)

  val comp      = Nonterminal('comp)

  val ifte      = Nonterminal('ifte)

  val aefs      = Nonterminal('aefs)     //first stage
  val aess      = Nonterminal('aess)     //second stage
  val aefs2     = Nonterminal('aefs2)     //first stage
  val aess2     = Nonterminal('aess2)     //second stage
  val add       = Nonterminal('add)
  val sub       = Nonterminal('sub)
  val mul       = Nonterminal('mul)
  val div       = Nonterminal('div)
  val add2      = Nonterminal('add2)
  val sub2      = Nonterminal('sub2)
  val mul2      = Nonterminal('mul2)
  val div2      = Nonterminal('div2)

  val num       = Terminal(digitsParser('num))
  val #+        = Comment(commentParser("+"))
  val #*        = Comment(commentParser("*"))
  val #-        = Comment(commentParser("-"))
  val #/        = Comment(commentParser("/"))
  val #==       = Comment(commentParser("=="))
  val ifK      = Comment(commentParser("if "))
  val thenK    = Comment(commentParser("then "))
  val elseK    = Comment(commentParser("else "))
  val eps       = Comment(epsilonParser)
  

  def epsilonParser: Parser[Tree] = 
    parseRegex("") ^^ {x => Leaf('eps, "")}
  
  def digitsParser(symbol: Symbol): Parser[Tree] =
    parseRegex("[0-9]+") ^^ { x => Leaf(symbol, x) }

  def digitsParserL(symbol: Symbol): Parser[Tree] =
    parseRegexL("[0-9]+") ^^ { x => Leaf(symbol, x) }

  def keywordParser(keyword: String): Parser[Tree] =
    parseRegex("\\s*\\Q" + keyword + "\\E\\s*") ^^ { x => Leaf('keyword, keyword) }

  def commentParser(keyword: String): Parser[Tree] =
    parseRegex("\\s*\\Q" + keyword + "\\E\\s*") ^^ { x => Leaf('comment, keyword) }

  def commentParserL(keyword: String): Parser[Tree] = 
    parseRegexL("\\s*\\Q" + keyword + "\\E\\s*") ^^ { x => Leaf('comment, keyword) }

  case class Grammar(start: Nonterminal, rules: Map[Nonterminal, RuleRHS]) {
    def lookup(nonterminal: Nonterminal): RuleRHS = rules(nonterminal)
  }

  val ae: Grammar =
    Grammar(
      start = exp,
      rules = Map(
        exp    -> (ifte || comp || aefs),
        ifte   -> (ifK ~ comp ~ thenK ~ exp ~ elseK ~ exp),
        comp   -> (aefs ~ #== ~ aefs),
        aefs   -> (add  || sub  || aess),
        aefs2  -> (add2 || sub2 || aess2),
        aess   -> (mul  || div  || num),
        aess2  -> (mul2 || div2 || num),
        add    -> (aess ~ #+ ~ aefs),
        add2   -> (aess ~ #- ~ aefs2),
        sub    -> (aess ~ #- ~ aefs2),
        sub2   -> (aess ~ #+ ~ aefs),
        mul    -> (num  ~ #* ~ aess),
        mul2   -> (num  ~ #/ ~ aess2),
        div    -> (num  ~ #/ ~ aess2),
        div2   -> (num  ~ #* ~ aess)
      )
    )
  

  /** Parsing the grammar of your choice.
    * Always produce simplified syntax trees.
    * Should not be hard-coded for arithmetic expressions.
    */

  def parseAE(input: String): Tree = simplifyAE({
    println("Parsing: " + input)
    var parser = parseNonterminal(ae.start, ae)
    parser(input) match {
      case Some((t, rest)) => {
        if(rest.isEmpty)
          t
        else{
          //sys.error("Rest not empty: " + rest)
          println("Rest: " + rest)
          t
        }
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
          t1 ::: t2
        }
      }

      case LSequence(s1, s2) => (parseRHS(s2, grammar) ~ parseRHS(s1, grammar)) ^^ {
        case (t1:List[Tree], t2:List[Tree]) => {
          t1 ::: t2
        }
      }
    
      case Select(c1, c2) => (parseRHS(c1, grammar) | parseRHS(c2, grammar)) 

      case Choice(c1, c2) => (parseRHS(c1, grammar) | parseRHS(c2, grammar))
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
      case Branch('add2, List(lhs, rhs)) =>
        eval(lhs) + eval(rhs)

      case Branch('sub, List(lhs, rhs)) =>
        eval(lhs) - eval(rhs)
      case Branch('sub2, List(lhs, rhs)) =>
        eval(lhs) - eval(rhs)

      case Branch('mul, List(lhs, rhs)) =>
        eval(lhs) * eval(rhs)
      case Branch('mul2, List(lhs, rhs)) =>
        eval(lhs) * eval(rhs)

      case Branch('div, List(lhs, rhs)) =>
        eval(lhs) / eval(rhs)
      case Branch('div2, List(lhs, rhs)) =>
        eval(lhs) / eval(rhs)

      case Leaf('num, code) =>
        code.toInt

      case Leaf('numL, code) =>
        code.toInt

      case Branch('comp, List(lhs, rhs)) => {
        if(eval(lhs) == eval(rhs))
          1
        else
          0
      }

      case Branch('ifte, List(ife, thene, elsee)) =>{
        if(eval(ife) == 1)
          eval(thene)
        else
          eval(elsee)
      }
    }

  def parseAndEval: String=>Int = eval _ compose parseAE _

}
