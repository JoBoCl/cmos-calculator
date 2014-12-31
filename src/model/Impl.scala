package model

/**
 * Created by joshua on 17/12/14.
 */
// These should not be created when parsing, but are only used by the cmosify function
case class Impl(lhs: Node, rhs: Node) extends Node {
  def get = !lhs.get || rhs.get
}
