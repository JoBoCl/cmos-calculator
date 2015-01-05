package helper

import model._

/**
 * Created by joshua on 17/12/14.
 */
object NormalForms {
  // TOOD: Fix this so that it actually does something
  // Pre : has been simplified as far as possible
  /*
  def convertToDnf(expr : model.Node): model.Node = expr match {
    case Variable(x) => expr
    case model.Constant(x) => expr
    case model.Not(a) => a match {
      model.Not(b) => convertToDnf(b)
      Variable(x) => model.Not(Variable(x))
      model.And(x, y) => model.Or(convertToDnf(model.Not(x)), convertToDnf(model.Not(y)))
      model.Or(x, y) => model.And(convertToDnf(model.Not(x)), convertToDnf(model.Not(y)))
    }
    case model.And(x, y) => {
      // if we've got model.And(x,y) all the way down, finish
      // otherwise, bring the model.Or up until it is at the root
      convertToDnf(x) match {
        Variable(x) => expr

      }
    }
    case model.Or(x, y) => {
    }
  }
  */

  // Pre : has been converted to sum of minterms form
  def cmosify(expr : Option[Node]) : Option[(Wire, Wire)] = expr match {
    case Some(x) =>
      Some(Tuple2(convertToCmos(x, true, None), convertToCmos(negate(x), false, None)))
    case None => None
  }

  def convertToCmos(expr : Node, fromSource : Boolean, direction : Option[Direction]) : Wire = Nil.asInstanceOf[Wire]

  /*  expr match {
      case And(x, y) => {
        val wire = new WireImpl()
        val cmosx = convertToCmos(x, fromSource, direction)
        val cmosy = convertToCmos(y, fromSource, direction)
        wire.addSource(cmosx)
        wire.addDrain(cmosy)
        wire
      }
      case Or(x, y) => {

        Nil.asInstanceOf[Wire]
      }
      case Variable(x) => {

        Nil.asInstanceOf[Wire]
      }
      case Not(Variable(x)) => {
        // Negations should only be attached to variables

        Nil.asInstanceOf[Wire]
      }
      case Constant(v) => {

        Nil.asInstanceOf[Wire]
      }
    }
*/

  // if And(a, b) then convert a to CMOS, find its drain, convert b to CMOS, find its source, then stick them both together
  // if Or(a, b) then convert a to CMOS, find its drain, convert b to CMOS, find its drain, and put them both on the same bit of wire
  // if Variable(x) then PGate or NGate
  //     // Should only be in this form, if not, something's gone wrong
  //     case Impl(e, Iff(a, b)) => e match {
  //       // Should only be attached to a variable
  //       case Not(Variable(x)) =>
  //         if (fromSource) {
  //           val ret = PGate(Variable(x), a, b)
  //           position match {
  //             case Some(RightOf(n)) =>
  //               ret.placement = Some(RightOf(n))
  //             case Some(Above(n)) =>
  //               ret.placement = Some(Above(n))
  //             case None =>
  //               ret.placement = None
  //           }
  //           ret
  //         } else {
  //           val ret = NGate(Not(Variable(x)), a, b)
  //           position match {
  //             case Some(RightOf(n)) =>
  //               ret.placement = Some(RightOf(n))
  //             case Some(Above(n)) =>
  //               ret.placement = Some(Below(n))
  //             case None =>
  //               ret.placement = None
  //           }
  //           ret
  //         }
  //       case Variable(x) =>
  //         if (fromSource) {
  //           val ret = PGate(Not(Variable(x)), a, b)
  //           position match {
  //             case Some(RightOf(n)) =>
  //               ret.placement = Some(RightOf(n))
  //             case Some(Above(n)) =>
  //               ret.placement = Some(Above(n))
  //             case None =>
  //               ret.placement = None
  //           }
  //           ret
  //         } else {
  //           val ret = NGate(Variable(x), a, b)
  //           position match {
  //             case Some(RightOf(n)) =>
  //               ret.placement = Some(RightOf(n))
  //             case Some(Above(n)) =>
  //               ret.placement = Some(Below(n))
  //             case None =>
  //               ret.placement = None
  //           }
  //           ret
  //         }
  //       case Or(x, y) => {
  //         val n1 = convertToCmos(Impl(x, Iff(a,b)))(fromSource)(position)
  //         val n2 = convertToCmos(Impl(y, Iff(a,b)))(fromSource)(Some(RightOf(n1)))
  //         And(n1, n2)
  //       }
  //       case And(x, y) => {
  //         val m = Variable.getUnusedVariable(!fromSource)
  //         val n1 = convertToCmos(Impl(x, Iff(a, m)))(fromSource)(position)
  //         val n2 = convertToCmos(Impl(y, Iff(m, b)))(fromSource)(Some(Above(n1)))
  //         And(n1, n2)
  //       }
  //     }

  // TODO: All of this.
  /*
    // Pre : has been simplified as far as possible
    def convertToCnf(expr : model.Node): model.Node = expr match {

    }
    */

  def negate(expr : Node) : Node = LogicalSimplifier.simplify(Not(expr))
}
