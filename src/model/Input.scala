package model

/**
 * Created by joshua on 17/12/14.
 */
trait Input {
  var sources = Array[CMOS]()
  def addSource(node : CMOS) = {
    sources = node +: sources
  }
}
