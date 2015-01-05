package model

/**
 * Created by joshua on 17/12/14.
 */
object Source extends Wire with Output {
  def get() : Potential = High()
}
