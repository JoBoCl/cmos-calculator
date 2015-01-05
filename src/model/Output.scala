package model

/**
 * Created by joshua on 17/12/14.
 */
trait Output {
  var drains = Array[Gate]()

  def addDrain(node : Gate) = {
    drains = node +: drains
  }
}
