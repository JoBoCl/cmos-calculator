package model

/**
 * Created by joshua on 17/12/14.
 */
case class Not(negated: Node) extends Node with Atom {
  def get = !negated.get
}
