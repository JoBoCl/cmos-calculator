package model

/**
 * Created by joshua on 17/12/14.
 */
object Source extends Wire with Output {
  override def removeGate (transistor : Transistor) : Unit = {
    drains = drains diff Array(transistor)
  }

  def get() : Potential = High()

  override def clear : Unit = clearDrains

  override def toString() = "Source"

  override def resetDrawnGates () : Unit = ()
}
