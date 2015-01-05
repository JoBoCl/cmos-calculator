package model

/**
 * Represents the current logical expression as a sum of products
 * (sum of minterms)
 */
class LogicalFunction {
  private[this] var minterms = List[List[Variable]]()
  private[this] var satisfying = Set[List[Variable]]()
  private[this] var numericalValues = List[Int]()

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

  def quineMcCluskey() : Unit = {
  }

  def findMinimalCoveringMinterms() : Unit = {
  }

  def convertToMinTerms(expr : Node) : Unit = {
    val identMap = Variable.getMap
    val size = 2 ^ identMap.size
    var variables = ""
    for (term <- identMap) {
      variables += term._1
    }
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
      }
    }
    qmm.method(numericalValues, Nil, qmm.letters(variables));
  }
}

