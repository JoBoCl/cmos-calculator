package helper

import model.{LogicalFunction, Node, Wire}

object Parser extends LogicalExpression {
  def cmos(x : String) : Option[(Wire, Wire)] =
    NormalForms.cmosify(variableParser(x))

  def convertExpressionToMinTerms(expr : Node) : Option[Node] = {
    LogicalFunction.quineMcCluskey(expr)
  }

  def main(args : Array[String]) {
    //this.reserved ++= Set(" and ", " or ", "!")
    println("testing parser")

    // TODO: Extract this to a proper testing rig
    println("Expected Result: none")
    println("Actual Result: " + variableParser("out"))
    println("Expected Result: a")
    println("Actual Result: " + variableParser("a"))
    println("Expected Result: !a")
    println("Actual Result: " + variableParser("! a"))
    println("Actual Result: " + variableParser("!a"))
    println("Actual Result: " + variableParser("!(a)"))
    println("Actual Result: " + variableParser("0"))
    println("Expected Result: 1")
    println("Actual Result: " + variableParser("!0"))
    println("Expected Result: 1")
    println("Actual Result: " + variableParser("1"))
    println("Expected Result: 0")
    println("Actual Result: " + variableParser("!1"))
    println("Expected Result: a")
    println("Actual Result: " + variableParser("!(!a)"))
    println("Expected Result: !a and !b")
    println("Actual Result: " + variableParser("!(a or b)"))
    println("Actual Result: " + variableParser("0 or a"))
    println("Expected Result: 1")
    println("Actual Result: " + variableParser("1 or a"))
    println("Expected Result: 0")
    println("Actual Result: " + variableParser("0 and a"))
    println("Expected Result: a")
    println("Actual Result: " + variableParser("1 and a"))
    println("Expected Result: 1")
    println("Actual Result: " + variableParser("(1 or a) and 1"))
    println("Expected Result: a or b")
    println("Actual Result: " + variableParser("a or b"))
    println("Expected Result: a and b")
    println("Actual Result: " + variableParser("a and b"))
    println("Expected Result: a")
    println("Actual Result: " + variableParser("a or a"))
    println("Expected Result: ((a or b) or c) and a")
    println("Actual Result: " + variableParser("((a or b) or c) and a"))
    println("Expected Result: (a and b) or ((a and b) or a)")
    println("Actual Result: " + variableParser("(a and b) or ((a and b) or a)"))
    println("Expected Result: a or b and c or (a and c) or a")
    println("Actual Result: " + variableParser("a or b and c or (a and c) or a"))
    println("Expected Result: a and b and a and c or b")
    println("Actual Result: " + variableParser("a and b and a and c or b"))
    println("Expected Result: a or b or d")
    println("Actual Result: " + variableParser("a or b or d"))
    println("Expected Result: a and b and c")
    println("Actual Result: " + variableParser("a and b and c"))
    println("Expected Result: ")
    println("Actual Result: " + variableParser(""))
  }

  def variableParser(x : String) : Option[Node] = {
    val result = parseAll(expr, x)
    if (result.successful)
      Some(result.get)
    else
      None
  }
}