// Based on github.com/jjfiv/qmm-scala

package model

import helper.Parser

import scala.annotation.tailrec
import scala.collection.immutable.List

object qmm {
  def main(args : Array[String]) {
    println(method(List(0, 1, 2, 3, 4, 7, 6, 8, 11, 13, 15), Nil, letters("ABCD")))
    println(method(List(1, 2, 3, 5, 9, 10, 11, 18, 19, 20, 21, 23, 25, 26, 27), Nil, letters("ABCDE")))
    //--- cyclic:
    println(method(List(8, 10, 16, 18, 19, 20, 21, 23, 25, 27, 29, 40, 42, 43, 46, 47, 55), Nil, letters("ABCDEF")))

    //--- don't cares
    println(method(List(1, 4, 7, 14, 17, 20, 21, 22, 23), List(0, 3, 6, 19, 30), letters("ABCDE")))
    //method(List(0,1,2,3),Nil,List("X","Y"))

    //method(List(0,1,2,3), Nil, letters("AB"))
    val res = method(List(1, 2), Nil, letters("AB"))
    method(List(0, 1, 2, 3), Nil, letters("AB"))
    println(Parser.variableParser(res))

    ()
  }

  def method(minterms : List[Int], dontcares : List[Int], vars : List[String]) : String = {
    val implicants = (minterms ::: dontcares).sorted.sortBy(bitcount(_)).map(new Implicant(_))
    val order = vars.length

    val prime_implicants = genImplicants(implicants, order).filter(_.prime)

    val results = PITable.solve(prime_implicants, minterms, vars)
    //println(results)

    val res = results.toSumOfProducts(vars)
    println(res)
    return res
  }

  def bitcount(x : Int) : Int = {
    @tailrec def bcrec(accum : Int, n : Int) : Int = n match {
      case 0 => accum
      case x => bcrec(accum + (x % 2), (x >>> 1))
    }
    bcrec(0, x)
  }

  def genImplicants(zero_cubes : List[Implicant], order : Int) : List[Implicant] = {

    import scala.collection.mutable.{Set => MutableSet}

    //--- generate list and populate with zero-cubes
    var implicants = MutableSet[Implicant]()
    for (i <- zero_cubes) implicants += i

    //--- operate on current order until highest reached
    for (currentOrder <- 0 until order) {
      //--- grab all implicants of the current order
      val data = implicants.toList.filter(_.order == currentOrder)

      for (List(a, b) <- data.combinations(2)) {
        if (a.canCombine(b)) {
          a.prime = false
          b.prime = false

          val n = a.combine(b)

          implicants += n
        }
      }
    }

    implicants.toList
  }

  def letters(x : String) : List[String] = x.split("").filter(_ != "").toList
}

