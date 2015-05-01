package model

/**
 * Created by joshua on 17/12/14.
 */
case class High() extends Driven with Potential {
  override def isHigh : Boolean = true
}
