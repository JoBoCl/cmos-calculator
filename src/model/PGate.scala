package model

import helper.Direction

/**
 * P(g, s, d) = ¬g -> (s <-> d)
 *
 * P gates can only carry high potential from the source to their drain
 * @param input
 * @param source
 * @param drain
 */
case class PGate(input : Node, source : Wire, drain : Wire) extends Gate {
  var placement : Option[Direction] = None

  def get = if (input.get) source.get() else Undriven()

  def drainDriven = input.get && (source.get == High())
}
