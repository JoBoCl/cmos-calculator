package model

/**
 * Created by joshua on 17/04/15.
 */
class WireLow extends Wire {
  override def removeGate (transistor : Transistor) : Unit = {
    sources = sources filter (x => !(x eq transistor))
    drains = drains filter (x => !(x eq transistor))
  }

  override def get() : Potential = {
    var res : Potential = Undriven()
    for (drain <- getDrains) {
      if (drain.get != Undriven()) {
        res = drain.get
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
    for (drain <- getDrains) {
      drain.resetDrawnGates()
    }
  }
}
