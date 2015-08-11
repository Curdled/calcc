package calcc

/**
 * Created by joe on 11/08/15.
 */
abstract class Token(pos: Int)

case class Plus(pos: Int) extends Token(pos)
case class Minus(pos: Int) extends Token(pos)
case class Multi(pos: Int) extends Token(pos)
case class Cos(pos: Int) extends Token(pos)
case class Fact(pos: Int) extends Token(pos)
case class Num(value:List[Char], pos: Int) extends Token(pos)
case class WS(pos :Int) extends Token(pos)