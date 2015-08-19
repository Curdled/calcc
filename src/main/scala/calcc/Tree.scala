package calcc


abstract class Tree[+A]{
  def print(indent: Int): String
}

case class Node[+A](h: A, t: List[Tree[A]]) extends Tree[A]{
  def wsIndent(i:Int): String ={
    if (i <= 0) ""
    else "-" + wsIndent(i-1)
  }
  def print(indent: Int): String ={
    wsIndent(indent) + h + "\n" + t.foldLeft("")((acc, tree) => acc + tree.print(indent+1))
  }
  override def toString = {
    print(0)
  }
}

object Leaf extends Tree{
  override def toString = ""

  override def print(indent: Int): String = ""
}
