package model

/**
 * Created by joshua on 17/12/14.
 */
abstract class Transistor {
  def remove () = {
    drain.removeGate(this)
    source.removeGate(this)
  }

  val input : Node
  val drain : Wire
  val source : Wire
  var drawnGate : Option[AnyRef]
  def resetDrawnGates() : Unit

  def get() : Potential
}
