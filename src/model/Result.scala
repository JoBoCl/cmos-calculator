package model

object Result extends Wire with Input with Output {
  override def get() : Potential = {
    var result : Potential = Undriven()
    for (gate <- getSources) {
      if (gate.get != Undriven()) {
        result = gate.get
      }
    }
    result
  }

  override def clear : Unit = {
    clearDrains
    clearSources
  }
}
