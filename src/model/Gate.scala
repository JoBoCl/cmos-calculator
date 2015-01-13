package model

/**
 * Created by joshua on 17/12/14.
 */
trait Gate {
  def get() : Potential

  //var wireToSource : Wire = Nil.asInstanceOf[Wire]

  //var wireToDrain : Wire = Nil.asInstanceOf[Wire]
}
