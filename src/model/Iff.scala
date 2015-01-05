package model

/**
 * Created by joshua on 17/12/14.
 */
case class Iff(lhs : Node, rhs : Node) extends Node {
  def get = (!lhs.get || rhs.get) && (!rhs.get || lhs.get)
}
