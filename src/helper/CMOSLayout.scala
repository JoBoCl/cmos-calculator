package helper

import model._

import scala.collection.mutable
import scala.collection.mutable.{HashSet, Stack}

/**
 * Created by joshua on 08/01/15.
 */
object CMOSLayout {

  private var totalGates = 0

  def main(args: Array[String]): Unit = {
    layout(helper.Parser.variableParser("(x and y) or ((!x) and (!y) and z)") match {
      case Some(v) => {
        println(v); v
      }
    })
    println(Result.getSources.length)
    for (source <- Result.getSources) println(source.toString)
    println(Result.getDrains.length)
    for (drain <- Result.getDrains) println(drain.toString)
  }

  private val negations = new HashSet[Variable]()

  def checkForDuplicateGates (wire : Wire)(checkingTopNetwork : Boolean) : Unit  = {
    // if we're at the result, we've finished
    if (wire != Result) {
      val wiresToCheck = new mutable.HashSet[Wire]()
      val gates = new mutable.HashMap[Node, Transistor]()
      for (gate <- (if(checkingTopNetwork) wire.getDrains else wire.getSources)) {
        if (gates.contains(gate.input)) {
          if (checkingTopNetwork) {
            for (drain <- gate.drain.getDrains) gates(gate.input).drain.addDrain(drain)
            wiresToCheck += gates(gate.input).drain
          } else {
            for (source <- gate.source.getSources) gates(gate.input).source.addSource(source)
            wiresToCheck += gates(gate.input).source
          }
          gate.remove()
          totalGates -= 1
        } else {
          gates.put(gate.input, gate)
        }
      }
      for (wire <- wiresToCheck) checkForDuplicateGates(wire)(checkingTopNetwork)
    }
  }

  def layout(expr: model.Node): String = {
    val normal = tryLayout(expr)
    val doubleNegate = tryLayout(Not(expr)) + 2

    if (normal <= doubleNegate) {
      tryLayout(expr)
      return normal + "transistors used"
    } else {
      return doubleNegate + "transistors used, with two to negate the output"
    }
  }

  def tryLayout(expr : model.Node) : Integer = {
    Result.clear;
    totalGates = 0;
    negations.clear()

    LogicalFunction.quineMcCluskey(expr) match {
      case Some(x) => {
        execute(x)(true)
      }
      case None => {
        throw new RuntimeException("Expression not minimised correctly")
      }
    }
    LogicalFunction.quineMcCluskey(Not(expr)) match {
      case Some(x) => {
        execute(x)(false)
      }
      case None => {
        throw new RuntimeException("Expression not minimised correctly")
      }
    }
    Variable.negateAll(expr)
    //checkForDuplicateGates(Source)(true)
    //checkForDuplicateGates(Drain)(false)
    (totalGates + 2 * negations.size)
  }

  /**
   * pre: expr has been converted to sum of products form (DNF)
   * @param expr - the expression to be implemented in Gui
   * @param buildingTopNetwork - indicates if the network we are building is to be used to carry a high potential or
   *                           a low potential
   * @return - side effects on the Result object
   */
  private def execute(expr: model.Node)(buildingTopNetwork: Boolean): Unit = {
    // I : \/ stack or subExpr = expr
    // Initially stack = [], subExpr = expr => I
    val stack = Stack[model.Node]()
    var subExpr = expr

    while (subExpr != Constant(false)) {
      subExpr match {
        case Or(x, y) => {
          stack push y
          subExpr = x
        }
        case Variable(_) => {
          println("Processing: " + subExpr.toString)
          convertToGates(subExpr, buildingTopNetwork)
          if (!stack.isEmpty) {
            subExpr = stack.pop()
          } else {
            subExpr = Constant(false)
          }
        }
        case Not(_) => {
          println("Processing: " + subExpr.toString)
          convertToGates(subExpr, buildingTopNetwork)
          if (!stack.isEmpty) {
            subExpr = stack.pop()
          } else {
            subExpr = Constant(false)
          }
        }
        case And(_, _) => {
          println("Processing: " + subExpr.toString)
          convertToGates(subExpr, buildingTopNetwork)
          if (!stack.isEmpty) {
            subExpr = stack.pop()
          } else {
            subExpr = Constant(false)
          }
        }
      }
    }
  }

  /**
   * As the parser operates using a fold, the right hand node must always be an Or(x,y), with the last node being made
   * up of a Variable(x) or Not(Variable(x))
   *
   * For some expression, assuming buildingTopNetwork, `$x_0 \land x_1 \land \ldots \land x_n$`, with literals `$x_0, x_1, \ldots x_n$`, `$x_0$`'s
   * drain is attached to the output wire, `$x_0$`'s source is attached to a new wire, which is then attached to `$x_1$`'s
   * drain, working along the conjunction until the last node.  `$x_n$` is attached to the source wire.
   *
   * If not buildingTopNetwork, replace all instances of source with drain, and attach `$x_n$` to the drain wire.
   *
   * @param node the logical expression to be converted to a series of gates
   * @param buildingTopNetwork indicates if the top half, or bottom is being evaluated
   *                           (fromBottom => PGate, !fromBottom => NGate)
   *
   *                           If a connection is made above a wire, then a source is being added, and if it is being made below a wire, then a
   *                           drain is added
   */
  private def convertToGates(node: Node, buildingTopNetwork: Boolean) = {
    var subNode: model.Node = node
    var currentWire: Wire = Result

    while (subNode != Constant(false)) subNode match {
      case And(x, y) => {
        val wire = if (buildingTopNetwork) { new WireHigh() } else { new WireLow() }
        x match {
          case Not(Variable(v)) => {
            if (buildingTopNetwork) {
              val gate = new PTrans(Variable(v), currentWire, wire)
              currentWire addSource gate
              wire addDrain gate
              println("Adding source to " + currentWire.toString())
            } else {
              val gate = new NTrans(Not(Variable(v)), wire, currentWire)
              negations add Variable(v)
              currentWire addDrain gate
              wire addSource gate
              println("Adding drain to " + currentWire.toString())
            }
          }
          case Variable(v) => {
            if (buildingTopNetwork) {
              val gate = new PTrans(Not(Variable(v)), currentWire, wire)
              negations add Variable(v)
              currentWire addSource gate
              wire addDrain gate
              println("Adding source to " + currentWire.toString())
            } else {
              val gate = new NTrans(Variable(v), wire, currentWire)
              currentWire addDrain gate
              wire addSource gate
              println("Adding drain to " + currentWire.toString())
            }
          }
        }
        currentWire = wire
        subNode = y
      }
      // Create a new gate and finish
      case Not(Variable(v)) => {
        if (buildingTopNetwork) {
          val gate = new PTrans(Variable(v), currentWire, Source)
          currentWire addSource gate
          Source addDrain gate
          println("Adding source to " + currentWire.toString())
        } else {
          val gate = new NTrans(Not(Variable(v)), Drain, currentWire)
          negations add Variable(v)
          currentWire addDrain gate
          Drain addSource gate
          println("Adding drain to " + currentWire.toString())
        }
        subNode = Constant(false)
      }
      case Variable(v) => {
        if (buildingTopNetwork) {
          val gate = new PTrans(Not(Variable(v)), currentWire, Source)
          negations add Variable(v)
          currentWire addSource gate
          Source addDrain gate
          println("Adding source to " + currentWire.toString())
        } else {
          val gate = new NTrans(Variable(v), Drain, currentWire)
          currentWire addDrain gate
          Drain addSource gate
          println("Adding drain to " + currentWire.toString())
        }
        subNode = Constant(false)
      }
    }
  }

  private def exprNotAtom(expr: Node) : Boolean = expr match {
    case And(_, _) => true
    case Or(_, _) => true
    case _ : Atom => false
    case _ => true
  }

  def exprNotConjunction(node: Node): Boolean = node match {
    case And(_, _) => false
    case _ => true
  }
}
