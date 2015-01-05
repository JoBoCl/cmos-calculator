package model

/**
 * Created by joshua on 17/12/14.
 */
case class Paren(expr : Node) extends Node {
  def get = expr.get
}
