package calcc

/**
 * Created by joeisaacs on 18/08/15.
 */

object E extends NonTerminal {
  override def toString = "E'"
}

object Sum extends NonTerminal {
  override def toString = "Sum'"
}
object Minus extends NonTerminal  {
  override def toString = "minus"
}
object Product extends NonTerminal  {
  override def toString = "product"
}
object Trig extends NonTerminal  {
  override def toString = "trig"
}

object Factorial extends NonTerminal {
  override def toString = "Factorial"
}

object Number extends NonTerminal {
  override def toString = "number"
}


object ParserProductions {
  val start    = new Production(E,          List(Sum),                      l => l.head)
  val splus    = new Production(Sum,        List(Sum, LPlus(0), Sum),        l => Node(LPlus(0), List(l(2), l.head)))
  val splus2   = new Production(Sum,        List(Minus),                    l => l.head)
  val sminus   = new Production(Minus,      List(Minus, LMinus(0), Product), l => Node(LMinus(0), List(l(2), l.head)))
  val sminus2  = new Production(Minus,      List(Product),                  l => l.head)
  val sprod    = new Production(Product,    List(Trig, LMulti(0), Product),  l => Node(LMulti(0), List(l(2), l.head)))
  val sprod2   = new Production(Product,    List(Trig),                     l => l.head)
  val strig    = new Production(Trig,       List(LCos(0), Trig),             l => Node(LCos(0), List(l.head)))
  val strig2   = new Production(Trig,       List(Factorial),                l => l.head)
  val sfact    = new Production(Factorial,  List(Number, LFact(0)),       l => Node(LFact(0), List(l(1))))
  val sfact2   = new Production(Factorial,  List(Number),                   l => l.head)
  val snum     = new Production(Number,     List(LPlus(0), LNum(List(),0)),    l => Node(LPlus(0), List(l.head)))
  val snum2    = new Production(Number,     List(LMinus(0), LNum(List(),0)),   l => Node(LMinus(0), List(l.head)))
  val snum3    = new Production(Number,     List(LNum(List(),0)),             l => l.head)

  val startItem = new GrammerItem(start, 0)

  val allProductions = Set(start, splus, splus2, sminus, sminus2, sprod, sprod2,
                           strig, strig2, sfact, sfact2, snum, snum2, snum3)

  val allGrammarSymbols = Set(E, Sum, Minus, Product, Trig, Factorial, Number, LPlus(0), LCos(0), LFact(0), LMinus(0), LMulti(0), LNum(List(),0), END)
}
