package calcc

/**
 * Created by joe on 11/08/15.
 */
abstract class LToken(val pos: Int) extends Terminal

case class LPlus(position: Int) extends LToken(position)
case class LMinus(position: Int) extends LToken(position)
case class LMulti(position: Int) extends LToken(position)
case class LCos(position: Int) extends LToken(position)
case class LFact(position: Int) extends LToken(position)
case class LNum(value:List[Char], position: Int) extends LToken(position)
case class LWS(position :Int) extends LToken(position)
case class EOF(position: Int) extends LToken(position)