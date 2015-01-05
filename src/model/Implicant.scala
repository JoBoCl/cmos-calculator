package model

/**
 * Created by joshua on 23/12/14.
 */
class Implicant(val minterm : Int, val tag : Int = 1, val group : List[Int] = Nil) {
  var prime : Boolean = true

  def cost(order : Int) : Int = {
    order - group.size
  }

  def order() : Int = {
    return group.length
  }

  def canCombine(other : Implicant) : Boolean = {
    //--- if the other one is less than this, don't bother comparing
    //if (other.minterm < minterm)
    //return false

    //--- only include ones that exist in at least one function
    if ((other.tag & tag) == 0)
      return false

    //--- if differences are not equivalent, don't bother comparing
    if (group != other.group)
      return false

    def bitdist(x : Int, y : Int) = qmm.bitcount(x ^ y)

    //--- difference needs to be just one bit
    if (bitdist(other.minterm, minterm) != 1)
      return false

    return true
  }

  override def equals(that : Any) = that match {
    case other : Implicant => {
      hashCode == other.hashCode
    }
    case _ => false
  }

  override def hashCode = terms().hashCode

  def terms() = {
    var terms : List[Int] = List(minterm)
    for (difference <- group) {
      terms = terms ::: terms.map(_ + difference)
    }
    terms
  }

  def combine(other : Implicant) : Implicant = {
    val newtag = other.tag & tag;
    val diff = math.abs(other.minterm - minterm)
    val newgroup = (group ::: List(diff)).sorted
    val newmt = if (minterm > other.minterm) other.minterm else minterm

    return new Implicant(newmt, newtag, newgroup)
  }

  override def toString() = terms().mkString("<", ",", ">")

  def printTerms() = println(terms().mkString("(", ",", ")"))

  def print() {
    printf("%d %s tag=%d %s\n", minterm, group.mkString("(", ",", ")"), tag, if (prime) {
      "prime"
    } else {
      ""
    })
  }

  def withVars(vars : List[String]) : String = {
    val weights = (0 until vars.length).map(1 << _).reverse
    val varByWeight = (weights zip vars).toMap
    //println(varByWeight)

    val expression = for (w <- weights) yield {
      if (!group.contains(w)) {
        if ((minterm & w) != 0) {
          varByWeight(w)
        } else {
          varByWeight(w) + "'"
        }
      } else {
        ""
      }
    }

    expression.filter(_ != "").reduceLeft(_ + _)
  }
}
