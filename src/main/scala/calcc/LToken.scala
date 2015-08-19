package calcc

/**
 * Created by joe on 11/08/15.
 */
abstract class LToken(pos: Int) extends Terminal(pos)

case class LPlus(poss: Int) extends LToken(poss){
  override def toString = "+"
}
case class LMinus(poss: Int) extends LToken(poss){
  override def toString = "-"
}
case class LMulti(poss: Int) extends LToken(poss){
  override def toString = "*"
}
case class LCos(poss: Int) extends LToken(poss){
  override def toString = "Cos"
}
case class LFact(poss: Int) extends LToken(poss){
  override def toString = "!"
}
case class LNum(l: List[Char], poss: Int) extends LToken(poss){
  def canEqual(a: Any) = a.isInstanceOf[LNum]

  override def equals(that: Any): Boolean =
    that match {
      case that: LNum =>  true
      case _ => false
    }
  override def toString = l.foldLeft("")((acc, s) => acc + s)
}
case class LWS(poss: Int) extends LToken(poss){
  override def toString = "SPACE"
}
