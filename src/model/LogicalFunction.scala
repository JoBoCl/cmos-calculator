package model

import helper.Parser

/**
 * Represents the current logical expression as a sum of products
 * (sum of minterms)
 */
object LogicalFunction {
  private[this] var minterms = List[List[Variable]]()
  private[this] var satisfying = Set[List[Variable]]()

  def get() : Boolean = {
    var result = false;
    for (term <- minterms) {
      var minres = true;
      for (atom <- term) {
        minres = minres && atom.get
      }
      result = result || minres
    }
    result
  }

  def quineMcCluskey(expr : Node) : Option[Node] = {
    Parser.variableParser(convertToMinTerms(expr))
  }

  def convertToMinTerms(expr : Node) : String = {
    var numericalValues = List[Int]()
    val tempMap = Variable.getMap
    val identMap = tempMap - "out"
    val size = 1 << identMap.size
    var variables = ""
    for (term <- identMap) {
      variables = term._1 + variables
    }
    println(variables)
    // iterate over every combination of values (2^n time)
    for (i <- 0 until size) {
      var j = i
      var terms = List[Variable]()
      for (term <- identMap) {
        if (j % 2 == 1) {
          terms = Variable(term._1) +: terms
          Variable.setValue(term._1, true)
        } else {
          Variable.setValue(term._1, false)
        }
        j = j / 2
      }
      if (expr.get) {
        satisfying = satisfying + terms
        numericalValues = i +: numericalValues
        println(i)
        println(for (term ← identMap) yield term._1 + ": " + Variable.lookup(term._1))
      }
    }
    return qmm.method(numericalValues, Nil, qmm.letters(variables))
  }

  def main(args : Array[String]) {
    //println(convertToMinTerms(Parser.variableParser("(a and b) or (a and !b) or (!a and b)") match { case Some(x) => x }))
    println(convertToMinTerms(Parser.variableParser("(a and !c) or (!a and b)") match { case Some(x) => x }))
  }
}


