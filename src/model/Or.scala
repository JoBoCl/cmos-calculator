package model

/**
 * Created by joshua on 17/12/14.
 */
case class Or(lhs : Node, rhs : Node) extends Node {
  def get = lhs.get || rhs.get
}
