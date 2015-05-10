package model

object Result extends Wire with Input with Output {
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
      throw new RuntimeException("Result is undriven!")
    } else {
      result
    }
  }

  override def clear : Unit = {
    clearDrains
    clearSources
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
