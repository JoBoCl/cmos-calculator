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
}


