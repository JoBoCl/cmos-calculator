package helper

import model._

/**
 * Created by joshua on 08/01/15.
 */
object CMOSLayout {

  def layout(expr : Node) : Unit = {
    Result.clear
    LogicalFunction.quineMcCluskey(expr) match {
      case Some(x) => { execute(x)(true) }
      case None => { throw new RuntimeException("Expression not minimised correctly") }
    }
    LogicalFunction.quineMcCluskey(Not(expr)) match {
      case Some(x) => { execute(x)(false) }
      case None => { throw new RuntimeException("Expression not minimised correctly") }
    }
    ()
  }
  /**
   * pre: expr has been converted to sum of products form (DNF)
   * @param expr - the expression to be implemented in CMOS
   * @param buildingTopNetwork - indicates if the network we are building is to be used to carry a high potential or
   *                           a low potential
   * @return - side effects on the Result object
   */
  private def execute(expr : Node)(buildingTopNetwork : Boolean) = {
    // I : \/ stack ∨ subexpr = expr
    // Initially stack = [], subexpr = expr => I
    val stack = scala.collection.mutable.Stack[Node]()
    var subexpr = expr

    while (exprNotAtom(subexpr) || !stack.isEmpty) {
      subexpr match {
        case Or(x, y) => {
          stack push y
          subexpr = x
        }
        case And(_, _) => {
          convertToGates(subexpr, buildingTopNetwork)
          subexpr = Nil.asInstanceOf[Node]
        }
        case _ => {
          subexpr = stack.pop()
        }
      }
    }
  }

  /**
   * As the parser operates using a fold, the right hand node must always be an Or(x,y), with the last node being made
   * up of a Variable(x) or Not(Variable(x))
   *
   * For some expression, assuming buildingTopNetwork, x_0 ∧ x_1 ∧ ... ∧ x_n, with literals x_0, x_1, ... x_n, x_0's
   * drain is attached to the output wire, x_0's source is attached to a new wire, which is then attached to x_1's
   * drain, working along the conjunction until the last node.  x_n is attached to the source wire.
   *
   * If not buildingTopNetwork, replace all instances of source with drain, and attach x_n to the drain wire.
   *
   * @param node the logical expression to be converted to a series of gates
   * @param buildingTopNetwork indicates if the top half, or bottom is being evaluated
   *                           (fromBottom => PGate, !fromBottom => NGate)
   *
   */
  private def convertToGates(node : Node, buildingTopNetwork : Boolean) = {
    var subnode = Nil.asInstanceOf[Node]
    var currentWire = Nil.asInstanceOf[WireImpl]
    node match {
      // x ∧ y
      case And(x, y) => {
        val wire = new WireImpl();
        x match {
          case Not(Variable(x)) => {
            if (buildingTopNetwork) {
              val gate = new PGate(Variable(x), wire, Result)
              wire addDrain gate
              Result addSource gate
            } else {
              val gate = new NGate(node, Result, wire)
              Result addSource gate
              wire addDrain gate
            }
          }
          case Variable(x) => {
            if (buildingTopNetwork) {
              val gate = new PGate(Not(Variable(x)), wire, Result)
              wire addDrain gate
              Result addSource gate
            } else {
              val gate = new NGate(node, Result, wire)
              Result addSource gate
              wire addDrain gate
            }
          }
        }
        currentWire = wire
        subnode = y
      }
      // Create a new gate and finish
      case Not(Variable(x)) => {
        if (buildingTopNetwork) {
          val gate = new PGate(Variable(x), Source, Result)
          Source addDrain gate
          Result addSource gate
        } else {
          val gate = new NGate(node, Result, Drain)
          Result addSource gate
          Drain addDrain gate
        }
      }
      case Variable(x) => {
        if (buildingTopNetwork) {
          val gate = new PGate(Not(Variable(x)), Source, Result)
          Source addDrain gate
          Result addSource gate
        } else {
          val gate = new NGate(node, Result, Drain)
          Result addSource gate
          Drain addDrain gate
        }
      }
    }

    while (exprNotAtom(subnode)) subnode match {
      case And(x, y) => {
        val wire = new WireImpl()
        x match {
          case Not(Variable(x)) => {
            if (buildingTopNetwork) {
              val gate = new PGate(Variable(x), currentWire, wire)
              currentWire addDrain gate
              wire addSource gate
            } else {
              val gate = new NGate(node, wire, currentWire)
              currentWire addSource gate
              wire addDrain gate
            }
          }
          case Variable(x) => {
            if (buildingTopNetwork) {
              val gate = new PGate(Not(Variable(x)), Source, Result)
              currentWire addDrain gate
              wire addSource gate
            } else {
              val gate = new NGate(node, Result, Drain)
              currentWire addSource gate
              wire addDrain gate
            }
          }
        }
        currentWire = wire
        subnode = y
      }
      // Create a new gate and finish
      case Not(Variable(x)) => {
        if (buildingTopNetwork) {
          val gate = new PGate(Variable(x), Source, currentWire)
          Source addDrain gate
          currentWire addSource gate
        } else {
          val gate = new NGate(node, Result, Drain)
          Result addSource gate
          Drain addDrain gate
        }
      }
      case Variable(x) => {
        if (buildingTopNetwork) {
          val gate = new PGate(Not(Variable(x)), Source, Result)
          Source addDrain gate
          Result addSource gate
        } else {
          val gate = new NGate(node, Result, Drain)
          Result addSource gate
          Drain addDrain gate
        }
      }
    }
  }

  private def exprNotAtom(expr : Node) : Boolean = expr match {
    case And(_, _) => true
    case Or(_, _) => true
    case _ : Atom => false
    case _ => true
  }
}
