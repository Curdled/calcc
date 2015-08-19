package calcc


import calcc.GrammarTable.LRTable


/**
 * Created by joeisaacs on 16/08/15.
 */



abstract class Action

case class Shift(state: Int) extends Action
case class Reduce(production: Production) extends Action
object Accept extends Action{
  override def toString = "acc"
}


class Parser(table:  LRTable){
  def parse(input: List[Terminal]) = {
    def parser(input: List[Terminal], stack: List[Int],t: List[Tree[GrammarSymbol]]): Tree[GrammarSymbol] = {
      val x = (input.head, stack.head)
      val u = table._1.get((input.head, stack.head))
      u match{
        case None => throw new IllegalArgumentException(s"No next state in the action table")// ${input.head.pos}")
        case Some(x) => x match{
          case Shift(y) => parser(input.tail, y :: stack, Node(input.head, List(Leaf)) :: t)
          case Reduce(y) =>
            val newStack = stack.drop(y.right.size)
            table._2.get((y.left, newStack.head)) match{
              case None => throw new IllegalArgumentException(s"No next state in the GOTO table")
              case Some(z) =>
                val (treeHead, treeTail) = t.splitAt(y.right.size)
                parser(input, z::newStack, Node[GrammarSymbol](y.left,treeHead.reverse) :: treeTail)
            }
          case Accept => t.head
        }
      }
    }
    parser(input ++ List(END), List(0), List(Leaf))//add the start state
  }

}