package calcc

/**
 * Created by joeisaacs on 18/08/15.
 */
abstract class Tree[+A]

case class Node[+A](h: A, t: List[Tree[A]]) extends Tree[A]

object Leaf extends Tree{
  override def toString = "Leaf"
}
