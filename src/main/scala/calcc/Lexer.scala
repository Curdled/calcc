package calcc

/**
 * Created by joe on 11/08/15.
 */
class Lexer(func: Map [(Int, List[Char]), Int], start: Int, finish: Map[Int, (List[Char], Int) => Token]) {

  def parse(values: String): Either[List[Token], String] = parse(values.toCharArray.toList)

  def parse(allValues: List[Char]): Either[List[Token], String] = {
    try {
      Left(parseWrapper(allValues))
    } catch {
      case e: IllegalArgumentException => Right(e.getLocalizedMessage)
    }
  }

  private def parseWrapper(allValues: List[Char]): List[Token] = {
    def parseHelper(values: List[Char], state: Int, prev: List[Char], pos: Int): List[Token] = {
      values match {
        case Nil =>
          // Check all the values where accepted.
          if(allValues.length == pos) List()
          else throw new IllegalArgumentException(s"Value at position ${ pos + 1 } not expected")
        case x :: xs => func filterKeys ((v: (Int, List[Char])) => v._1 == state && v._2.contains(x)) headOption match {
          case Some(y) => finish get y._2 match {
            case Some(z) =>
              // Try to match the next element.
              if (nextValueTrue(y._2, xs)) {
                // The current state is an accepting state.
                parseHelper(xs, y._2, x :: prev, pos + 1)
              }
              else {
                z((x :: prev).reverse, pos - prev.size) :: parseHelper(xs, start, List(), pos + 1)
              }
            case None => { // The current state is not an accepting state
              if (nextValueTrue(y._2, xs))
                parseHelper(xs, y._2, x :: prev, pos + 1)
              else
                throw new IllegalArgumentException(s"Value at position ${ pos + 1 } not expected" )
            }
          }
            // A next state value cannot be found.
          case None => throw new IllegalArgumentException(s"Value at position ${ pos + 1 } not expected")
        }
      }
    }

    def nextValueTrue(state: Int, passed: List[Char]): Boolean = {
      if (passed.isEmpty) false
      else {
        (func filterKeys ((v: (Int, List[Char])) => v._1 == state && v._2.contains(passed.head))).nonEmpty
      }
    }
    parseHelper(allValues, start, List(), 0)
  }
}
