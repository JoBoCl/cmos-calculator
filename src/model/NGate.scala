package model

import helper.Direction

/**
 * N(g, s, d) = g -> (s <-> d)
 *
 * N gates can only carry low potential from their drain to their source
 */
case class NGate(input : Node, drain : Wire, source : Wire) extends Gate {
  override def get : Potential = if (input.get) drain.get() else Undriven()

  def sourceDriven = input.get && (drain.get == Low())
}
