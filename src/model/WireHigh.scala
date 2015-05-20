package model

/**
 * Created by joshua on 17/04/15.
 */
class WireHigh extends Wire with Input with Output {
  override def removeGate (transistor : Transistor) : Unit = {
    sources = sources filter (x => !(x eq transistor))
    drains = drains filter (x => !(x eq transistor))
  }

  override def get() : Potential = {
    var res : Potential = Undriven()
    for (source <- getSources) {
      if (source.get != Undriven()) {
        res = source.get
      }
    }
    res
  }

  override def clear : Unit = {
    clearSources
    clearDrains
  }

  override def toString() = ""

  override def resetDrawnGates() : Unit = {
    for (source <- getSources) {
      source.resetDrawnGates()
    }
  }
}
