// Based on github.com/jjfiv/qmm-scala

package model

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.immutable.List

object qmm {
  def bitcount(x: Int): Int = {
    @tailrec def bcrec(accum: Int, n: Int): Int = n match {
      case 0 => accum
      case x => bcrec(accum + (x % 2),(x>>>1))
    }
    bcrec(0, x)
  }
  def genImplicants(zero_cubes: List[Implicant], order: Int): List[Implicant] = {

    import scala.collection.mutable.{Set => MutableSet}

    //--- generate list and populate with zero-cubes
    var implicants = MutableSet[Implicant]()
    for(i <- zero_cubes) implicants += i

    //--- operate on current order until highest reached
    for(currentOrder <- 0 until order) {
      //--- grab all implicants of the current order
      val data = implicants.toList.filter(_.order == currentOrder)

      for(List(a, b) <- data.combinations(2)) {
        if(a.canCombine(b)) {
          a.prime = false
          b.prime = false

          val n = a.combine(b)

          implicants += n
        }
      }
    }

    implicants.toList
  }

  def method(minterms: List[Int], dontcares: List[Int], vars: List[String]) {
    val implicants = (minterms:::dontcares).sorted.sortBy(bitcount(_)).map(new Implicant(_))
    val order = vars.length

    val prime_implicants = genImplicants(implicants, order).filter(_.prime)

    val results = PITable.solve(prime_implicants, minterms, vars)
    //println(results)

    println(results.toSumOfProducts(vars))

  }

  def letters(x: String): List[String] = x.split("").filter(_ != "").toList

  def main(args: Array[String]) {
    method(List(0,1,2,3,4,7,6,8,11,13,15), Nil, letters("ABCD"))
    method(List(1,2,3,5,9,10,11,18,19,20,21,23,25,26,27),Nil,letters("ABCDE"))
    //--- cyclic:
    method(List(8,10,16,18,19,20,21,23,25,27,29,40,42,43,46,47,55), Nil, letters("ABCDEF"))

    //--- don't cares
    method(List(1,4,7,14,17,20,21,22,23), List(0,3,6,19,30), letters("ABCDE"))
    //method(List(0,1,2,3),Nil,List("X","Y"))
  }
}


//--- class that slowly covers less and less
class PrimeImplicant(val implicant: Implicant, val terms: Set[Int]) {
  val tag = implicant.tag
  val order = implicant.order()

  //--- remove given terms from this data's effect
  def reduce(coveredTerms: Set[Int]) = {
    new PrimeImplicant(implicant, terms -- coveredTerms)
  }
  //--- if this is higher order and contains all the minterms of the other
  def dominates(other: PrimeImplicant) = ((other.order <= order) && (other.terms.subsetOf(terms)))
  //--- whether this contains a given minterm or not
  def covers(minterm: Int) = terms.contains(minterm)
  //--- whether this is now empty
  def empty() = terms.size == 0

  override def equals(that: Any) = that match {
    case other: PrimeImplicant => hashCode == other.hashCode
    case _ => false
  }
  //override def hashCode = implicant.hashCode
  override def toString() = terms.mkString("",",","")
}

object PITable {
  def solve(primeImplicants: List[Implicant], minterms: List[Int], vars: List[String]) = {

    val start = new PITable(
      primeImplicants.map(x => new PrimeImplicant(x, x.terms().toSet)),
      minterms.toSet,
      Set[Implicant](),
      vars
    )

    bestSolution(start)
  }
  //@tailrec
  def bestSolution(t: PITable): PITable = t.finished match {
    case true => t
    case false => {
      val branches = for(row <- t.rows) yield bestSolution(reduceTable(t.selectRow(row)))
      branches.minBy(_.cost(t.vars.length))
    }
  }
  @tailrec def reduceTable(t: PITable): PITable = t.selectEssential match {
    case (true, newTable) => reduceTable(newTable.reduceRows)
    case (false, _) => t.reduceRows
  }
}

case class PITable(val rows: List[PrimeImplicant], val cols: Set[Int], val results: Set[Implicant], val vars: List[String]) {

  def cost(order: Int) = {
    results.foldLeft(0){_ + _.cost(order)}
  }
  def finished = cols.size == 0
  def selectRow(row: PrimeImplicant) = {
    val nRows = rows.filter(_ != row).map(_.reduce(row.terms))
    val nCols = cols -- row.terms
    val nRes  = results + row.implicant
    this.copy(rows=nRows, cols=nCols, results=nRes)
  }

  def reduceRows = {
    var nRows = rows.filter(!_.empty())
    for(List(a,b) <- rows.combinations(2)) {
      if(a dominates b) {
        nRows = nRows.filter(_ != b)
      } else if(b dominates a) {
        nRows = nRows.filter(_ != a)
      }
    }
    this.copy(rows=nRows)
  }

  def rowsForMinterm(m: Int) = (for (row <- rows if row.covers(m)) yield row).filter(_ != ())

  def selectEssential = {
    var newTable = this
    var done = false
    var effective = false

    while(!done) {
      done = true
      for (m <- cols) {
        newTable.rowsForMinterm(m) match {
          case List(x: PrimeImplicant) => {
            done = false
            effective = true
            newTable = newTable.selectRow(x)
          }
          case _ => ()
        }
      }
    }

    (effective, newTable)
  }

  def toSumOfProducts(vars: List[String]) = {
    assert(finished)

    results.map(_.withVars(vars)).toList.sorted.reduceLeft(_ + " + " + _)
  }

}
