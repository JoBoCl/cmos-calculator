package model

/**
 * P(g, s, d) = ¬g -> (s <-> d)
 *
 * P gates can only carry high potential from the source to their drain
 */
case class PGate(input : Node, drain : Wire, source : Wire) extends Gate {
  override def get : Potential = if (!input.get) source.get() else Undriven()
  var drawnGate : Option[AnyRef] = None

  def drainDriven = input.get && (source.get == High())

  override def resetDrawnGates() : Unit = {
    drawnGate = None
    source.resetDrawnGates()
  }
}
