package calcc



/**
 * Created by joeisaacs on 16/08/15.
 */


abstract class Action

case class Shift(state: Int) extends Action
case class Reduce(production: Production) extends Action
object Accept extends Action{
  override def toString = "acc"
}
case class Error(state: Int) extends Action