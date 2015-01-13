package model

import scala.annotation.tailrec

/**
 * Created by joshua on 05/01/15.
 */
object PITable {
  def solve(primeImplicants : List[Implicant], minterms : List[Int], vars : List[String]) = {

    val start = new PITable(
      primeImplicants.map(x => new PrimeImplicant(x, x.terms().toSet)),
      minterms.toSet,
      Set[Implicant](),
      vars
    )

    bestSolution(start)
  }

  //@tailrec
  def bestSolution(t : PITable) : PITable = t.finished match {
    case true => t
    case false => {
      val branches = for (row <- t.rows) yield bestSolution(reduceTable(t.selectRow(row)))
      branches.minBy(_.cost(t.vars.length))
    }
  }

  @tailrec def reduceTable(t : PITable) : PITable = t.selectEssential match {
    case (true, newTable) => reduceTable(newTable.reduceRows)
    case (false, _) => t.reduceRows
  }
}

case class PITable(val rows : List[PrimeImplicant], val cols : Set[Int], val results : Set[Implicant], val vars : List[String]) {

  def cost(order : Int) = {
    results.foldLeft(0) {
      _ + _.cost(order)
    }
  }

  def finished = cols.size == 0

  def selectRow(row : PrimeImplicant) = {
    val nRows = rows.filter(_ != row).map(_.reduce(row.terms))
    val nCols = cols -- row.terms
    val nRes = results + row.implicant
    this.copy(rows = nRows, cols = nCols, results = nRes)
  }

  def reduceRows = {
    var nRows = rows.filter(!_.empty())
    for (List(a, b) <- rows.combinations(2)) {
      if (a dominates b) {
        nRows = nRows.filter(_ != b)
      } else if (b dominates a) {
        nRows = nRows.filter(_ != a)
      }
    }
    this.copy(rows = nRows)
  }

  def rowsForMinterm(m : Int) = (for (row <- rows if row.covers(m)) yield row).filter(_ !=())

  def selectEssential = {
    var newTable = this
    var done = false
    var effective = false

    while (!done) {
      done = true
      for (m <- cols) {
        newTable.rowsForMinterm(m) match {
          case List(x : PrimeImplicant) => {
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

  def toSumOfProducts(vars : List[String]) = {
    assert(finished)

    results.map(_.withVars(vars)).toList.sorted.foldLeft("")((l, r) => if (l != "") "(%s) or (%s)".format(l, r) else r)
  }
}
