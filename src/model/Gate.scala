package model

/**
 * Created by joshua on 17/12/14.
 */
abstract class Gate  {
  val input : Node
  val drain : Wire
  val source : Wire
  def get() : Potential
}
