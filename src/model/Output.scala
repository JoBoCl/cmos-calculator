package model

/**
 * Created by joshua on 17/12/14.
 */
trait Output {
  var drains = Array[CMOS]()
  def addDrain(node : CMOS) = {
    drains = node +: drains
  }
}
