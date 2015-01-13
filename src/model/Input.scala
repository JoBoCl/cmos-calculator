package model

/**
 *
 */
trait Input {
  private var sources = Array[Gate]()

  def addSource(node : Gate) = {
    sources = node +: sources
  }

  def getSources = sources

  def clearSources : Unit = { sources =  Array[Gate]() }
}
