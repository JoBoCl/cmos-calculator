package model

abstract class Wire {
  private var sources = Array[Transistor]()
  private var drains = Array[Transistor]()

  def get() : Potential

  def addSource(node : Transistor) = {
    sources = node +: sources
  }

  def getSources = sources

  def clearSources : Unit = {
    sources = Array[Transistor]()
  }

  def clear() : Unit

  def getDrains = drains

  def addDrain(node : Transistor) = {
    drains = node +: drains
  }

  def clearDrains : Unit = {
    drains = Array[Transistor]()
  }

  def resetDrawnGates() : Unit
}




