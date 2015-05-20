/**
 * Created by joshua on 31/12/14.
 */

package model.test

import helper._
import model._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{FlatSpec, Matchers}

class ParserTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with TableDrivenPropertyChecks {

  val varNameA ="a"
  val b ="b"
  val test = "test"
  val tests = Table(("input", "output"),
                    ("(!a) and !b", Some(And(Not(Variable(varNameA)), Not(Variable(b))))),
                    ("(!a) and b", Some(And(Not(Variable(varNameA)), Variable(b)))),
                    ("a and !b", Some(And(Variable(varNameA), Not(Variable(b))))),
                    ("a and (!b)", Some(And(Variable(varNameA), Not(Variable(b))))),
                    ("a and b", Some(And(Variable(varNameA), Variable(b)))),
                    ("0", Some(Constant(false))),
                    ("1", Some(Constant(true))),
                    ("", None),
                    ("out", None),
                    ("(", None),
                    (")", None),
                    ("()", None),
                    ("!", None),
                    ("and", None),
                    ("a and", None),
                    ("and a", None),
                    ("or", None),
                    ("a or", None),
                    ("or a", None),
                    ("!a and !b", Some(Not(And(Variable(varNameA), Not(Variable(b)))))),
                    ("!a and b", Some(Not(And(Variable(varNameA), Variable(b))))),
                    ("!a or !b", Some(Not(Or(Variable(varNameA), Not(Variable(b)))))),
                    ("!a or b", Some(Not(Or(Variable(varNameA), Variable(b))))),
                    ("!a", Some(Not(Variable(varNameA)))),
                    ("(!a)", Some(Not(Variable(varNameA)))),
                    ("!(a)", Some(Not(Variable(varNameA)))),
                    ("(!a) or !b", Some(Or(Not(Variable(varNameA)), Not(Variable(b))))),
                    ("(!a) or b", Some(Or(Not(Variable(varNameA)), Variable(b)))),
                    ("a or !b", Some(Or(Variable(varNameA), Not(Variable(b))))),
                    ("a or (!b)", Some(Or(Variable(varNameA), Not(Variable(b))))),
                    ("a or b", Some(Or(Variable(varNameA), Variable(b)))),
                    ("a", Some(Variable(varNameA))),
                    ("(a)", Some(Variable(varNameA))),
                    ("test", Some(Variable(test)))
    )

    "The table of results" should  "be parsed as expected" in forAll (tests) {
                   (input : String, res : Option[Node]) => {
                     Parser.variableParser(input) should be (res)
                     Variable.clear()
                   }
                 }
}


