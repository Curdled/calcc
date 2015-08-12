package calcc

/**
 * Created by joe on 11/08/15.
 */
abstract class LToken(pos: Int)

case class LPlus(pos: Int) extends LToken(pos)
case class LMinus(pos: Int) extends LToken(pos)
case class LMulti(pos: Int) extends LToken(pos)
case class LCos(pos: Int) extends LToken(pos)
case class LFact(pos: Int) extends LToken(pos)
case class LNum(value:List[Char], pos: Int) extends LToken(pos)
case class LWS(pos :Int) extends LToken(pos)