package helper

import model._

/**
 * Created by joshua on 17/12/14.
 */
object LogicalSimplifier {
  def simplify(expr: Node): Node = expr match {
    case Variable(_) => expr
    case Constant(_) => expr
    case Not(x) => x match {
      case Variable(_) => expr
      // T => F, F => T
      case Constant(x) => Constant(!x)
      // !(!(y)) = y
      case Not(y) => simplify(y)
      // De Morgan's
      case And(y, z) => Or(simplify(Not(y)), simplify(Not(z)))
      case Or(y, z) => And(simplify(Not(y)), simplify(Not(z)))
    }
    case Or(x, y) => simplify(x) match {
      // Identity, 1 and 0 laws
      case Constant(true) => Constant(true)
      case Constant(false) => y match {
        case Constant(true) => Constant(true)
        case Constant(false) => Constant(false)
        case _ => simplify(y)
      }
      case _ => if (y == x) x else Or(simplify(x), simplify(y))
    }
    case And(x, y) => simplify(x) match {
      // Identity, 1 and 0 laws
      case Constant(false) => Constant(false)
      case Constant(true) => simplify(y) match {
        case Constant(true) => Constant(true)
        case Constant(false) => Constant(false)
        case _ => simplify(y)
      }
      case _ => if (y == x) x else And(simplify(x), simplify(y))
    }
  }
}
