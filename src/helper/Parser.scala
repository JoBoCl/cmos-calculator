package helper

import model.Node

object Parser extends LogicalExpression {
  def variableParser(x : String) : Option[Node] = {
    val result = parseAll(expr, x)
    if (result.successful)
      Some(result.get)
    else
      None
  }
}