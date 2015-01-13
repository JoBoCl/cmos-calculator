/**
 * Created by joshua on 31/12/14.
 */

package model.test

import helper._
import model._
import org.scalatest.{FlatSpec, Matchers}

class ParserTest extends FlatSpec with Matchers {

  "The empty string" should "be parsed as nothing" in {
    Parser.variableParser("") should be(None)
  }

  "a and b" should "be parsed as Some(And(Variable(a), Variable(b)))" in {
    Parser.variableParser("a and b") should be(Some(And(Variable("a"), Variable("b"))))
    Variable.clear()
  }

  "a and !a" should "be parsed as Some(And(Variable(a), Not(Variable(a)))) and then simplified to Some(Constant(false))" in {
    Parser.variableParser("a and !a") should be(Some(And(Variable("a"), Not(Variable("a")))))
    LogicalSimplifier.simplify(Parser.variableParser("a and !a") match { case Some(x : Node) => x }) should be(
      Constant(false)
    )
    Variable.clear()
  }
}

class QuineMcCluskeyTest extends FlatSpec with Matchers {
  private val test : String = "(!a and b and !c and !d) or (a and !b and !c and !d) or (a and !b and c and !d) or (a and !b and c and d) or (a and b and !c and !d) or (a and b and c and d)"

  "a and b" should "be reduced to Some(And(a), And(b))" in {
    val expr = Parser.variableParser("a and b") match {
      case Some(x) => x
    }
    LogicalFunction.quineMcCluskey(expr) should be(Some(
      And(Variable("a"), Variable("b"))
    )
    )
  }

  test should "be parsed as Some(...)" ignore {
    LogicalFunction.quineMcCluskey(Parser.variableParser(test) match { case Some(x) => x }) should be(Some(
      Variable("x")
    )
    )
    Variable.clear()
  }
}
