package model

/**
 * Created by joshua on 05/01/15.
 */
//--- class that slowly covers less and less
class PrimeImplicant(val implicant : Implicant, val terms : Set[Int]) {
  val tag = implicant.tag
  val order = implicant.order()

  //--- remove given terms from this data's effect
  def reduce(coveredTerms : Set[Int]) = {
    new PrimeImplicant(implicant, terms -- coveredTerms)
  }

  //--- if this is higher order and contains all the minterms of the other
  def dominates(other : PrimeImplicant) = ((other.order <= order) && (other.terms.subsetOf(terms)))

  //--- whether this contains a given minterm or not
  def covers(minterm : Int) = terms.contains(minterm)

  //--- whether this is now empty
  def empty() = terms.size == 0

  override def equals(that : Any) = that match {
    case other : PrimeImplicant => hashCode == other.hashCode
    case _ => false
  }

  //override def hashCode = implicant.hashCode
  override def toString() = terms.mkString("", ",", "")
}
