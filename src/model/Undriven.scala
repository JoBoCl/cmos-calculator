package model

/**
 * Created by joshua on 17/12/14.
 */
case class Undriven() extends Potential {
  override def isHigh : Boolean = false
}
