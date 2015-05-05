package model.test

import helper.Parser
import model.{And, LogicalFunction, Variable}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by joshua on 04/02/15.
 */
class QuineMcCluskeyTest extends FlatSpec with Matchers {
  private val test : String = "(!a and b and !c and !d) or (a and !b and !c and !d) or (a and !b and c and !d) or (a " +
    "and !b and c and d) or (a and b and !c and !d) or (a and b and c and d)"

  "a and b" should "be reduced to Some(And(a), And(b))" in {
    val expr = Parser.variableParser("a and b") match {
      case Some(x) => x
    }
    LogicalFunction.quineMcCluskey(expr) should be(Some(
      And(Variable("a"), Variable("b"))
    )
    )
  }

  test should "be parsed as Some(...)" in {
    LogicalFunction.quineMcCluskey(Parser.variableParser(test) match { case Some(x) => x }) should be(Some(
      Variable("x")
    )
    )
    Variable.clear()
  }
}
