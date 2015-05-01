package model

import scala.collection.immutable.TreeMap

case class Variable(val ident : String) extends Node with Atom {
  def get = Variable.lookup(ident)

  override def toString = ident
}

object Variable {
  def negateAll(expr : Node) : Unit = {
    for (variable <- identMap) {
      setValue(variable._1, false)
    }
    setValue("out", expr.get())
  }

  private var identMap = new TreeMap[String, Boolean]
  identMap += Tuple2("out", false)
  private var intermediate = 0;

  def getUnusedVariable(startsOn : Boolean) : Variable = {
    // Cannot parse expressions containing _, so guaranteed to be free
    identMap += Tuple2("i_" + intermediate, startsOn)
    intermediate += 1
    Variable("i_" + (intermediate - 1))
  }

  def create(ident : String, value : Boolean) : Variable = {
    if (!(identMap contains ident))
      identMap += Tuple2(ident, value)
    Variable(ident)
  }

  def lookup(ident : String) = identMap apply ident

  def setValue(ident : String, value : Boolean) = identMap += Tuple2(ident, value)

  def clear() = {
    identMap = new TreeMap[String, Boolean]
    identMap += Tuple2("out", false)
    intermediate = 0
  }

  def getMap = identMap
}


