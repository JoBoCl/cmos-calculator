package model

/**
 * Created by joshua on 17/12/14.
 */
case class Constant(val truth : Boolean) extends Node with Atom {
  def get = truth
}
