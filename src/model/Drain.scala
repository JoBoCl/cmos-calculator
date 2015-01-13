package model

/**
 * Created by joshua on 17/12/14.
 */
object Drain extends Wire with Output {
  def get() : Potential = Low()

  override def clear : Unit = clearDrains
}
