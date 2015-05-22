package model.test

import helper.Parser
import model._
import org.scalatest.prop.{TableDrivenPropertyChecks, GeneratorDrivenPropertyChecks}
import org.scalatest.prop.Tables.Table
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by joshua on 04/02/15.
 */
class QuineMcCluskeyTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with TableDrivenPropertyChecks {
  val varNameA ="a"
  val b ="b"
  val c = "c"
  val test = "test"
  val tests = Table(("input", "output"),
                    ("(!a) and a", None),
                    ("(!a) or a", None),
                    ("a and a and a and a", Some(Variable(varNameA))),
                    ("a or a or a or a", Some(Variable(varNameA))),
                    ("a and b", Some(And(Variable(varNameA), Variable(b)))),
                    ("(a and (!b)) or ((!a) and b)",
                        Some(Or(And(Variable(varNameA), Not(Variable(b))), And(Not(Variable(varNameA)), Variable(b))))),
                    ("!((a and b) or ((!a) and (!b) and c))", 
                        Some(Or(Or(And(Not(Variable(b)),Variable(varNameA)),And(Not(Variable(c)),Not(Variable(varNameA)))),And(Variable(b),Not(Variable(varNameA)))))),
                    ("a and b and c", Some(And(Variable(c),And(Variable(b),Variable(varNameA))))),
                    ("c and b and a", Some(And(Variable(c),And(Variable(b),Variable(varNameA))))),
                    ("(a and b and c) or (a and b and c)", Some(And(Variable(c),And(Variable(b),Variable(varNameA)))))
  )

  "The table of results" should  "be minimised as expected" in forAll (tests) { (input : String, res : Option[Node]) => {
      LogicalFunction.quineMcCluskey(Parser.variableParser(input).get) should be (res)
      Variable.clear()
    }
  }
}
