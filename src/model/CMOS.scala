package model

/**
 * Created by joshua on 17/12/14.
 */
case class CMOS(top: Node, bottom: Node) {

  def get: Potential = {
    val highPotentialDriven = getTop(top)
    val lowPotentialDriven = getBottom(bottom)
    // why is this here?  What does it all mean?  Variable.lookup("out")
    if (highPotentialDriven) return High()

    if (lowPotentialDriven) return Low()

    throw new RuntimeException("Cannot determine if driven")
  }

  def getTop(node: Node): Boolean = {
    true
  }

  def getBottom(node: Node): Boolean = {
    false
  }

  //   // Iterate over top half to set out
  //   // out may not be set, in which case, it should be in setBottom
  //   // PGates only propagate high current
  //   private def getTop(current : Node) : Unit = current match {
  //     case And(l : Node, r : Node) => {
  //       getTop(l) ; getTop(r)
  //     }
  //     case PGate(Not(Variable(g)), s, d) => if (!Variable.lookup(g)) {
  //       s match {
  //         case Not(Variable(x)) => if (!Variable.lookup(x)) {
  //           d match {
  //             case Variable(y) => Variable.setValue(y, true)
  //           }
  //         }
  //         case Variable(x) => if (Variable.lookup(x)) {
  //           d match {
  //             case Variable(y) => Variable.setValue(y, true)
  //           }
  //         }
  //         case Constant(true) =>
  //           d match {
  //             case Variable(y) => Variable.setValue(y, true)
  //           }
  //       }
  //     }
  //     case PGate(Variable(g), s, d) => if (Variable.lookup(g)) {
  //       s match {
  //         case Not(Variable(x)) => if (!Variable.lookup(x)) {
  //           d match {
  //             case Variable(y) => Variable.setValue(y, true)
  //           }
  //         }
  //         case Variable(x) => if (Variable.lookup(x)) {
  //           d match {
  //             case Variable(y) => Variable.setValue(y, true)
  //           }
  //         }
  //         case Constant(true) => {
  //           d match {
  //             case Variable(y) => Variable.setValue(y, true)
  //           }
  //         }
  //       }
  //     }
  //   }
  //
  //   private def getBottom(current : Node) : Unit = current match {
  //     case And(l : Node, r : Node) => {
  //       getBottom(l) ; getBottom(r)
  //     }
  //     case NGate(Not(Variable(g)), s, d) => if (!Variable.lookup(g)) s match {
  //       case Not(Variable(x)) => if (!Variable.lookup(x)) {
  //         d match {
  //           case Variable(y) => Variable.setValue(y, false)
  //         }
  //       }
  //       case Variable(x) => if (Variable.lookup(x)) {
  //         d match {
  //           case Constant(false) => Variable.setValue(x, false)
  //           case Variable(y) => Variable.setValue(y, false)
  //         }
  //       }
  //       case Constant(false) => d match {
  //         case Variable(y) => Variable.setValue(y, false)
  //       }
  //     }
  //     case NGate(Variable(g), s, d) => if (Variable.lookup(g)) {
  //       d match {
  //         case Variable(x) => if (Variable.lookup(x)) {
  //           s match {
  //             case Variable(y) => Variable.setValue(y, false)
  //           }
  //         }
  //         case Constant(false) => {
  //           s match {
  //             case Variable(y) => Variable.setValue(y, false)
  //           }
  //         }
  //       }
  //     }
  //   }
}
