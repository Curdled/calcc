package calcc

import calcc.GrammarTable.LRTable


/**
 * Created by joeisaacs on 16/08/15.
 */

abstract class GrammarSymbol

abstract class Terminal extends GrammarSymbol
abstract class NonTerminal extends GrammarSymbol

object END extends Terminal


case class Production(left: NonTerminal, right: List[GrammarSymbol],f: List[Tree[GrammarSymbol]] => Tree[GrammarSymbol]){
  override def toString =  left + " -> " + right.foldLeft("")((acc, s) => acc + s)


}

case class GrammerItem(production: Production, dotPos: Int){
  def nextSymbol = production.right.lift(dotPos)

  def moveDot = new GrammerItem(production, dotPos+1)

  def canMoveDot = production.right.size > dotPos

  def dotAtEnd = dotPos == production.right.size

  override def toString = production.left + "->" +  List(production.right.take(dotPos), List("."), production.right.drop(dotPos)).flatten.foldLeft("")((acc, s) => acc + s)
}

object GrammarTable{
  type LRTable = (Map[(Terminal, Int), Action], Map[(NonTerminal, Int),  Int])
}

class GrammarTable(G: Set[Production], ALL: Set[GrammarSymbol]){


  def Closure(I: Set[GrammerItem]): Set[GrammerItem] = {
    def computeClosure(J: Set[GrammerItem]): Set[GrammerItem] = {
      (for{
        j <- J
        g <- G
        if j.nextSymbol.contains(g.left)
      } yield new GrammerItem(g, 0)) ++ J
    }
    val J = computeClosure(I)
    if(J == I) J
    else Closure(J)
  }

  def GOTO(I: Set[GrammerItem], X: GrammarSymbol): Set[GrammerItem] ={
      Closure(for{
      i <- I
      if i.canMoveDot && i.nextSymbol.contains(X)
    } yield i.moveDot)
  }

  def CanonicalSet(startState: GrammerItem): Map[Set[GrammerItem], Int] = {
    val C = Set(Closure(Set(startState)))

    def computeItems(I: Set[Set[GrammerItem]]): Set[Set[GrammerItem]] = {
      val J = (for {
                     i <- I
                     x <- ALL
                     u = GOTO(i, x)
                     if u.nonEmpty && !I.contains(u)
                } yield u ) ++ I
      if (J == I) J
      else computeItems(J)
    }
    val cono = computeItems(C)
    val m = Map(Closure(Set(startState)) -> 0)
    cono.filter(p => !p.contains(startState)).foldLeft(m)((A, B) => A ++ Map(B -> A.size))
  }



  def grammarTable(startState: GrammerItem): LRTable = {
    val states = CanonicalSet(startState)
    def terminalStateToAction(t : Terminal, s: Set[GrammerItem]): Option[Action] = {
      if (t == END && s.contains(startState.moveDot)) Some(Accept)
      else if (GOTO(s, t).nonEmpty) Some(Shift(states(GOTO(s, t))))
      else if (s.foldLeft(false)((acc, I) => acc || I.dotAtEnd)) Some(Reduce(s.filter(i => i.dotAtEnd).head.production))
      else None
    }
    val goto = for{
      t <- ALL.collect{case t: NonTerminal => t}
      s <- states.keySet
      if GOTO(s, t) != Set()
    } yield (t, states(s)) -> states(GOTO(s, t))

    val action = for{
      t <- ALL.collect{case t: Terminal => t}
      s <- states.keySet
      v = terminalStateToAction(t,s)
      if v.isDefined
      Some(a) = v
    } yield (t, states(s)) -> a

    (action.toMap,  goto.toMap)
  }
}