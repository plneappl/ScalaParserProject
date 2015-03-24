/** Exercise 3.1: Export 2.2
  *
  * Tasks
  *
  * 1. Adapt your parser combinators from exercise 2.2
  *    and put them here.
  *
  * 2. [Optional] Read `parseRegex` and learn about variadic
  *    pattern matching.
  */


package util

trait Combinators {
  def choice[A](firstTry: => Parser[A], secondTry: => Parser[A]): Parser[A] =
    input => firstTry(input) match {
      case Some(success) =>
        Some(success)

      case None =>
        secondTry(input)
    }

  def sequence[A, B](parseFirstPart: => Parser[A], parseSecondPart: => Parser[B]): Parser[(A, B)] =
    input => parseFirstPart(input) match {
      case Some((firstResult, afterFirstPart)) =>
        parseSecondPart(afterFirstPart) match {
          case Some((secondResult, afterSecondPart)) =>
            Some( ((firstResult, secondResult), afterSecondPart) )

          case None =>
            None
        }

      case None =>
        None
    }

  def postprocess[A, B](parser: => Parser[A])(postprocessor: A => B): Parser[B] =
    input => parser(input) match {
      case Some( (result, rest) ) =>
        Some( (postprocessor(result), rest) )

      case None =>
        None
    }


  type Parser[A] = String => Option[(A, String)]

  implicit class ParserOps[A](self: => Parser[A]) {
    def | (that: => Parser[A]): Parser[A] =
      choice(self, that)

    def ~ [B] (that: => Parser[B]): Parser[(A, B)] =
      sequence(self, that)

    def ^^ [B] (postprocessor: A => B): Parser[B] =
      postprocess(self)(postprocessor)

  /* Sequence two parsers, ignore the second parser's result */
    def <~ [B] (that: => Parser[B]): Parser[A] =
      (this ~ that) ^^ {
        case (a, b) => a
      }

    /* Sequence two parsers, ignore the first parser's result */
    def ~> [B] (that: => Parser[B]): Parser[B] =
      (this ~ that) ^^ {
        case (a, b) => b
    }

  }

  def parseRegexL(regex: String) : Parser[String] = code => {
    val Pattern = s"(.*)($regex)".r
    code match {
      case Pattern(groups @ _*) =>
        Some((groups.last, groups.head))

      case otherwise =>
        None
    }
  }

  /** @param regex: a Java regular expression
    * @return a parser for all strings matched by regex
    *
    * For example, we may reimplement `parseNum` from 2.1
    * thus:
    *
    *   val parseNum = parseRegex("[0-9]+") ^^ {
    *     case digits => Num(digits.toInt)
    *   }
    */
  def parseRegex(regex: String): Parser[String] = code => {
    // construct a new regular expression for all strings
    // that match `regex` at the start
    val Pattern = s"($regex)(.*)".r

    code match {
      // If there are 4 groups in the regular expression
      // `Pattern`, then we can write
      //
      //   case Pattern(x1, x2, x3, x4) => ...
      //
      // to extract the 4 groups. However, since part
      // of the regular expression `Pattern` is given by
      // the user, we don't know exactly how many groups
      // there will be. We can write
      //
      //   case Pattern(groups @ _*) => ...
      //
      // instead, so that `groups` stands for the sequence
      // of all matched groups in the regular expression
      // `pattern`:
      //
      //   groups == Seq(x1, x2, x3, x4)
      //
      // The way we construct `Pattern` makes sure that the
      // first group always matches `regex` as a whole,
      // and the last group always matches the rest of the
      // string.
      //
      // Learn more about the language feature @ _* here:
      //
      //   http://stackoverflow.com/a/9229677

      case Pattern(groups @ _*) =>
        Some((groups.head, groups.last))

      case otherwise =>
        None
    }
  }

  def parseString(expected: String): Parser[String] = code => {
    if (code startsWith expected)
      Some((expected, code drop expected.length))
    else
      None
  }
}
