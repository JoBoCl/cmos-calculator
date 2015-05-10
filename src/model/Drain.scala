package model

/**
 * Created by joshua on 17/12/14.
 */
object Drain extends Wire with Input {
  def get() : Potential = Low()

  override def clear : Unit = clearSources

  override def toString() = "Drain"

  override def resetDrawnGates () : Unit = ()
}
