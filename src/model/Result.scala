package model

object Result extends Wire {
  override def removeGate (transistor : Transistor) : Unit = {
    sources = sources diff Array(transistor)
    drains = drains diff Array(transistor)
  }

  override def get() : Potential = {
    var result : Potential = Undriven()
    for (gate <- getSources) {
      if (gate.get != Undriven()) {
        result = gate.get
      }
    }
    if (result == Undriven()) {
      for (gate <- getDrains) {
        if (gate.get != Undriven()) {
          result = gate.get
        }
      }
    }
    if (result == Undriven()) {
      result//throw new RuntimeException("Result not driven")
    } else {
      result
    }
  }

  override def clear : Unit = {
    clearDrains
    clearSources
    Source.clear
    Drain.clear
  }

  override def toString() = "Result"

  override def resetDrawnGates() : Unit = {
    for (gate <- getSources) {
      gate.resetDrawnGates()
    }
    for (gate <- getDrains) {
      gate.resetDrawnGates()
    }
    ()
  }
}
