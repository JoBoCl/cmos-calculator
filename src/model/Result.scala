package model

object Result extends Wire with Input {
  override def get() : Potential = {
    var result : Potential = Undriven()
    for (gate <- getSources) {
      if (gate.get != Undriven()) {
        result = gate.get
      }
    }
    result
  }
}
