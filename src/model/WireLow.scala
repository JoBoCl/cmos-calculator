package model

/**
 * Created by joshua on 17/04/15.
 */
class WireLow extends Wire with Input with Output {
  override def get(): Potential = {
    var res: Potential = Undriven()
    for (drain <- getDrains) {
      if (drain.get != Undriven()) {
        res = drain.get
      }
    }
    res
  }

  override def clear: Unit = {
    clearSources
    clearDrains
  }

  override def toString() = ""
}
