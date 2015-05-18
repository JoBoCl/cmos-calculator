package helper

import model._

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

/**
 * \Sigma = [A-Za-z0-9]*
 * expr -> conj | disj | literal
 * conj -> literal | literal "and" conj
 * disj -> conj | conj "or" disj
 * literal -> "!" simp | simp
 * simp -> variable | constant | "(" expr ")" | expr
 * variable -> \Sigma\Sigma*\{"out", "0", "1"}
 * constant -> 0 | 1
 **/
class LogicalExpression extends RegexParsers with PackratParsers {
  lazy val variable : PackratParser[Node] = """^(?!out|and|or$)([A-Za-z0-9]+)""".r ^^ {
    v => {
      Variable.create(v, false)
    }
  }

  lazy val const : PackratParser[Node] = ("1" | "0") ^^ {
    case "1" => {
      Constant(true)
    }
    case "0" => {
      Constant(false)
    }
  }

  lazy val literal : PackratParser[Node] = ("!" ~ simp | simp) ^^ {
    case "!" ~ (n : Node) => {
      Not(n)
    }
    case n : Node => {
      n
    }
  }

  lazy val conj : PackratParser[Node] = rep1sep(literal, "and") ^^ {
    _.reduceRight(And(_, _))
  }

  lazy val disj : PackratParser[Node] = rep1sep(conj, "or") ^^ {
    _.reduceRight(Or(_, _))
  }

  lazy val simp : PackratParser[Node] = (variable
                                         ||| const
                                         ||| ("(" ~ expr ~ ")" ^^ {
    case "(" ~ e ~ ")" => {
      e
    }
  })
                                         ||| expr)

  lazy val expr : PackratParser[Node] = (conj
                                         ||| disj
                                         ||| literal)
}
