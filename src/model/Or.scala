package model

/**
 * Created by joshua on 17/12/14.
 */
case class Or(lhs : Node, rhs : Node) extends Node {
  def get = lhs.get || rhs.get

  // Allows for commutativity of operators
  override def equals(that : Any) : Boolean = that match {
    case Or(lhs_, rhs_) => (lhs_ == lhs && rhs_ == rhs) || (lhs_ == rhs && rhs_ == lhs)
    case _ => false
  }

  override def toString() = (lhs match {
    case Variable(x) => x;
    case _ => "(" + lhs.toString + ")";
  }) + " or " + (rhs match {
    case Variable(x) => x;
    case _ => "(" + rhs.toString + ")";
  })
}
