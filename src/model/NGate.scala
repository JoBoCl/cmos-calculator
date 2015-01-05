package model

import helper.Direction

/**
 * N(g, s, d) = g -> (s <-> d)
 *
 * N gates can only carry low potential from their drain to their source
 * @param input
 * @param source
 * @param drain
 */
case class NGate(input : Node, source : Wire, drain : Wire) extends Gate {
  var placement : Option[Direction] = None

  def get = if (input.get) drain.get() else Undriven()

  def sourceDriven = input.get && (drain.get == Low())
}
