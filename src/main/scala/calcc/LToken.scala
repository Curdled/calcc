package calcc

/**
 * Created by joe on 11/08/15.
 */
abstract class LToken extends Terminal

case class LPlus(pos: Int) extends LToken{
  override def toString = "+"
}
case class LMinus() extends LToken{
  override def toString = "-"
}
case class LMulti() extends LToken{
  override def toString = "*"
}
case class LCos() extends LToken{
  override def toString = "Cos"
}
case class LFact() extends LToken{
  override def toString = "!"
}
case class LNum(l: List[Char]) extends LToken{
  def canEqual(a: Any) = a.isInstanceOf[LNum]

  override def equals(that: Any): Boolean =
    that match {
      case that: LNum =>  true
      case _ => false
    }
  override def toString = l.foldLeft("")((acc, s) => acc + s)
}
case class LWS() extends LToken{
  override def toString = "SPACE"
}
