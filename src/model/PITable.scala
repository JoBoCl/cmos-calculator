package model

/**
 * Created by joshua on 23/12/14.
 */
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
