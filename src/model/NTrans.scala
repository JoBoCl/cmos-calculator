package model

/**
 * N(g, s, d) = g -> (s <-> d)
 *
 * N gates can only carry low potential from their drain to their source
 */
case class NTrans(input : Node, drain : Wire, source : Wire) extends Transistor {
  override def get : Potential = if (input.get) drain.get() else Undriven()
  var drawnGate : Option[AnyRef] = None

  def sourceDriven = input.get && (drain.get == Low())

  override def resetDrawnGates() : Unit = {
    drawnGate = None
    drain.resetDrawnGates()
  }
}
