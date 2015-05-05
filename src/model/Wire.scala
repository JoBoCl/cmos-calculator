package model

abstract class Wire {
  private var sources = Array[Gate]()
  private var drains = Array[Gate]()

  def get() : Potential

  def addSource(node : Gate) = {
    sources = node +: sources
  }

  def getSources = sources

  def clearSources : Unit = {
    sources = Array[Gate]()
  }

  def clear() : Unit

  def getDrains = drains

  def addDrain(node : Gate) = {
    drains = node +: drains
  }

  def clearDrains : Unit = {
    drains = Array[Gate]()
  }
}




