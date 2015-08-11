package calcc

/**
 * Created by joe on 11/08/15.
 */
class Lexer(func: Map [(Int, List[Char]), Int], start: Int, finish: Map[Int, (List[Char], Int) => Token]) {
  def parse(allValues: List[Char]): List[Token] = {
    def parse2(values: List[Char], state: Int, prev: List[Char], pos: Int): List[Token] = {
      values match {
        case Nil => List()
        case x :: xs => func filterKeys ((v: (Int, List[Char])) => v._1 == state && v._2.contains(x)) headOption match {
          case Some(y) => finish get y._2 match {
            case Some(z) =>
              if (nextValueTrue(y._2, xs)) {
                parse2(xs, y._2, x :: prev, pos + 1)
              }
              else {
                z((x :: prev).reverse, pos - prev.size) :: parse2(xs, start, List(), pos + 1)
              }
            case None => {
              if (nextValueTrue(y._2, xs))
                parse2(xs, y._2, x :: prev, pos + 1)
              else
                throw new Exception(s"Value at position ${ pos + 1 } not expected" )
            }
          }
          case None => List()
        }
      }
    }

    def nextValueTrue(state: Int, passed: List[Char]): Boolean = {

      if (passed.isEmpty) false
      else {
        (func filterKeys ((v: (Int, List[Char])) => v._1 == state && v._2.contains(passed.head))).nonEmpty
      }
    }
    parse2(allValues, start, List(), 0)
  }

  def parse(values: String): List[Token] = parse(values.toCharArray.toList)
}
