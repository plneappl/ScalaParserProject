
import sext._
import scala.collection.mutable.ListBuffer

object SimpleGrammar extends util.Combinators {
  sealed trait Tree
  case class Leaf(symbol: Symbol, code: String) extends Tree
  case class Branch(symbol: Symbol, children: List[Tree]) extends Tree

  sealed trait RuleRHS {
    def | (rhs: RuleRHS) = Choice(this, rhs)
    def || (rhs: RuleRHS) = Select(this, rhs)
    def ~ (rhs: RuleRHS) = Sequence(this, rhs)
    def ~~ (rhs: RuleRHS) = AutoSequence(this, rhs)
  }

  case class Nonterminal(symbol: Symbol) extends RuleRHS
  case class Terminal(parse: Parser[Tree]) extends RuleRHS
  case class Choice(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS
  case class Sequence(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS
  case class AutoSequence(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS
  case class Comment(parse: Parser[Tree]) extends RuleRHS
  case class Select(lhs: RuleRHS, rhs: RuleRHS) extends RuleRHS

  val exp       = Nonterminal('exp)
  val comp      = Nonterminal('comp)
  val ifte      = Nonterminal('ifte)
  val aefs      = Nonterminal('aefs)     //first stage
  val aess      = Nonterminal('aess)     //second stage
  val add       = Nonterminal('add)
  val sub       = Nonterminal('sub)
  val mul       = Nonterminal('mul)
  val div       = Nonterminal('div)

  val num       = Terminal(digitsParser('num))
  val #+        = Comment(commentParser("+"))
  val #*        = Comment(commentParser("*"))
  val #-        = Comment(commentParser("-"))
  val #/        = Comment(commentParser("/"))
  val #==       = Comment(commentParser("=="))
  val ifK       = Comment(commentParser("if "))
  val thenK     = Comment(commentParser("then "))
  val elseK     = Comment(commentParser("else "))
  val eps       = Comment(epsilonParser)
  

  def epsilonParser: Parser[Tree] = 
    parseRegex("") ^^ {x => Leaf('eps, "")}
  
  def digitsParser(symbol: Symbol): Parser[Tree] =
    parseRegex("[0-9]+") ^^ { x => Leaf(symbol, x) }

  def keywordParser(keyword: String): Parser[Tree] =
    parseRegex("\\s*\\Q" + keyword + "\\E\\s*") ^^ { x => Leaf('keyword, keyword) }

  def commentParser(keyword: String): Parser[Tree] =
    parseRegex("\\s*\\Q" + keyword + "\\E\\s*") ^^ { x => Leaf('comment, keyword) }

  case class Grammar(start: Nonterminal, rules: Map[Nonterminal, RuleRHS], 
    associativity: Map[Nonterminal, Boolean], 
    precedence: Map[Nonterminal, Int]) {
    def lookup(nonterminal: Nonterminal): RuleRHS = rules(nonterminal)
  }

  val lrG: Grammar = 
    Grammar(
      start = add,
      rules = Map(
        add -> ((add ~ #+ ~ add) || mul),
        mul -> ((mul ~ #* ~ mul) || num)
      ),
      associativity = Map(),
      precedence = Map()
    )
    
  def eliminateLR(lr: Grammar): Grammar = 
    Grammar(
      start = lr.start,
      rules = transformRules(lr.rules),
      associativity = lr.associativity,
      precedence = lr.precedence
    )

  def transformRules(m: Map[Nonterminal, RuleRHS]): Map[Nonterminal, RuleRHS] = {
    m.toList.map(x => x match {
        case (n, s:Choice) => {
          println("choice:")
          println(n)
          println(s)
          transformLRRule(n, collapseChoice(s), joinToChoice)
        }
        case (n, s:Select) => {
          println("select:")
          println(n)
          println(s)
          transformLRRule(n, collapseSelect(s), joinToSelect)
        }
        case (n, m) => {
          println("other:")
          println(n)
          println(m)
          Map(n -> m)
        }
      }).fold(Map())((a, b) => a ++ (b.toList))
    
  }

  //takes a rule of the form  lhs -> rhses, where rhses is a list of possible right hand sides: lhs -> rhs1 | rhs2 | rhs3 | ...
  //also see: http://www.csd.uwo.ca/~moreno//CS447/Lectures/Syntax.html/node8.html
  def transformLRRule(lhs: Nonterminal, rhses: List[RuleRHS], join: List[RuleRHS] => RuleRHS): Map[Nonterminal, RuleRHS] = {
    var alphas = ListBuffer[RuleRHS]()
    var betas = ListBuffer[RuleRHS]()
    for(rhs <- rhses) {
      rhs match {
        case t:Terminal => betas += t
        case c:Comment => betas += c
        case nt:Nonterminal if(nt != lhs) => betas += nt
        case s:Sequence => {
          val collapsed = collapseSequence(s)
          println(collapsed.head)
          if(collapsed.head == lhs){
            alphas += joinToSequence(collapsed.tail)
          }
          else{
            betas += s 
          }
        } 
      }
    }
    
    var secondNT = nextNonterminal(lhs)
    var lhsTo = join(betas.toList.map {
      r => (r ~~ secondNT)
    })
    var sNTTo = join(alphas.toList.map {
      r => (r ~~ secondNT)
    } :+ eps)
    Map(
      lhs -> lhsTo,
      secondNT -> sNTTo  
    )
  }
  
  def collapseSelect(s: RuleRHS): List[RuleRHS] = s match {
    case Select(s1, s2) => collapseSelect(s1) ++ collapseSelect(s2)
    case other => List(other)
  }
  
  def joinToSelect(s: List[RuleRHS]): RuleRHS = s match {
    case List(r1, r2) => (r1 || r2)
    case List(r) => r
    case r1 :: r => (r1 || joinToSelect(r))    
  }
  
  def collapseChoice(s: RuleRHS): List[RuleRHS] = s match {
    case Choice(s1, s2) => collapseChoice(s1) ++ collapseChoice(s2)
    case other => List(other)
  }
  
  def joinToChoice(s: List[RuleRHS]): RuleRHS = s match {
    case List(r1, r2) => (r1 | r2)
    case List(r) => r
    case r1 :: r => (r1 | joinToChoice(r))    
  }
  
  def collapseSequence(s: RuleRHS): List[RuleRHS] = s match {
    case Sequence(s1, s2) => collapseSequence(s1) ++ collapseSequence(s2)
    case other => List(other)
  }
  
  def joinToSequence(s: List[RuleRHS]): RuleRHS = s match {
    case List(r1, r2) => (r1 ~ r2)
    case List(r) => r
    case r1 :: r => (r1 | joinToSequence(r))    
  }
  
  def nextNonterminal(nt: Nonterminal): Nonterminal = 
    Nonterminal(Symbol(nt.symbol.toString))

  val ae: Grammar =
    Grammar(
      start = exp,
      rules = Map(
        exp    -> (ifte || comp || aefs),
        ifte   -> (ifK ~ comp ~ thenK ~ exp ~ elseK ~ exp),
        comp   -> (aefs ~ #== ~ aefs),
        aefs   -> (add  || sub  || aess),
        aess   -> (mul  || div  || num),
        add    -> (aess ~ #+ ~ aefs),
        sub    -> (aess ~ #- ~ aefs),
        mul    -> (num  ~ #* ~ aess),
        div    -> (num  ~ #/ ~ aess)
      ),
      associativity = Map(
        add -> true,
        sub -> true,
        mul -> true,
        div -> true
      ),
      precedence = Map(
        ifte -> 3,
        comp -> 2,
        add -> 1,
        sub -> 1,
        mul -> 0,
        div -> 0
      )
    )

  /** Parsing the grammar of your choice.
    * Always produce simplified syntax trees.
    * Should not be hard-coded for arithmetic expressions.
    */

  def parse(g: Grammar)(input: String): Tree = simplify(g)({
    println("Parsing: " + input)
    var parser = parseNonterminal(g.start, g)
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

  def parseAE(input: String): Tree = parse(ae)(input)

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
        List.empty 
      }})

      case Terminal(parser) => (parser ^^ { t => {
        List(t) 
      }})      

      case Sequence(s1, s2) => (parseRHS(s1, grammar) ~ parseRHS(s2, grammar)) ^^ {
        case (t1:List[Tree], t2:List[Tree]) => {
          t1 ::: t2
        }
      }
      
      case AutoSequence(s1, s2) => (parseRHS(s1, grammar) ~ parseRHS(s2, grammar)) ^^ {
        case (t1:List[Tree], t2:List[Tree]) => {
          t1 ::: t2
        }
      }
    
      case Select(c1, c2) => (parseRHS(c1, grammar) | parseRHS(c2, grammar)) 
      case Choice(c1, c2) => (parseRHS(c1, grammar) | parseRHS(c2, grammar))
    }
  
  def simplify(g: Grammar)(syntaxTree: Tree): Tree = syntaxTree match {
    case Branch(symbol, list) => {
      val list2 = list.filter(x => x match{case Branch(sym, List()) => false; case _ => true})
      var rule = g.lookup(Nonterminal(symbol))
      rule match {
        case Select(_, _) | AutoSequence(_, _) => {
          if(list2.length == 1){
            simplify(g)(list2.head)
          }
          else
            Branch(symbol, list2.map(simplify(g)))
        }
        case t => Branch(symbol, list2.map(simplify(g)))
      }
    }
    case t => t 
  }

  def simplifyAE(syntaxTree: Tree): Tree = simplify(ae)(syntaxTree)

  def transformAssoc(grammar: Grammar): Tree => Tree = syntaxTree => syntaxTree match {
    case t@(Branch(op1, list@(List(t1, Branch(op2, List(t2, t3)))))) 
      if(grammar.associativity(Nonterminal(op1)) && 
         grammar.associativity(Nonterminal(op2)) &&
         grammar.precedence(Nonterminal(op1)) == grammar.precedence(Nonterminal(op2))) => {
        transformAssoc(grammar)(Branch(op1, List(Branch(op2, List(transformAssoc(grammar)(t1), transformAssoc(grammar)(t2))), t3)))
      
    }
    case Branch(op1, list) => Branch(op1, list.map(transformAssoc(grammar)))
    case t => t
  }

  def transformAE: Tree => Tree = transformAssoc(ae)

  def eval(t: Tree): Int = t match {
      case Branch('add, List(lhs, rhs)) =>
        eval(lhs) + eval(rhs)
      case Branch('sub, List(lhs, rhs)) =>
        eval(lhs) - eval(rhs)
      case Branch('mul, List(lhs, rhs)) =>
        eval(lhs) * eval(rhs)
      case Branch('div, List(lhs, rhs)) =>
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

  def parseAndEval: String => Int = eval _ compose parseAE _

}
