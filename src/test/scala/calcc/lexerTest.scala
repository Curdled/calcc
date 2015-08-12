package calcc

import org.scalatest.{BeforeAndAfter, FunSuite}

import scala.collection.convert.Wrappers.MutableBufferWrapper

/**
 * Created by joe on 11/08/15.
 */
class lexerTest extends FunSuite with BeforeAndAfter {



  val tranisitions = Map(
    (0,List('+'))  -> 1,
    (0,List('-'))  -> 2,
    (0,List('*'))  -> 3,
    (0,List('!'))  -> 4,


    (0, List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')) -> 5,

    (5, List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')) -> 5,



    (5, List('.')) -> 6,
    (5, List('E', 'e')) -> 11,



    (6, List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')) -> 7,

    (7, List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')) -> 7,

    (7, List('E','e')) -> 8,

    (8, List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')) -> 10,
    (8, List('+', '-')) -> 9,


    (9, List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')) -> 10,

    (11, List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')) -> 12,
    (12, List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')) -> 12,

    (0, List('c','C')) -> 21,
    (21, List('o','O')) -> 22,
    (22, List('s','S')) -> 23,

    (0, List(' ', '\t')) -> 1000,
    (1000, List(' ', '\t')) -> 1000
  )

  val start = 0

  val finish:Map[Int, (List[Char], Int) => Token] =
    Map(1  -> {(_,x) => Plus(x)},
        2  -> {(_,x) => Minus(x)},
        3  -> {(_,x) => Multi(x)},
        4  -> {(_,x) => Fact(x)},
        5  -> {(a,x) => Num(a, x)},
        6  -> {(a,x) => Num(a, x)},
        7  -> {(a,x) => Num(a, x)},
        10 -> {(a,x) => Num(a, x)},
        12 -> {(a,x) => Num(a,x)},
        23 -> {(w,x) => Cos(x)},
        1000-> {(w,x) => WS(x)}

  )

  val l = new Lexer(tranisitions, start, finish)


  test("Parsing + - ! *") {
    assert(l.parse("+") match{
      case Left(v) => v == List(Plus(0))
      case Right(_) => false
    })

    assert(l.parse("+-!*++") match{
      case Left(v) =>  v == List(Plus(0), Minus(1), Fact(2), Multi(3), Plus(4), Plus(5))
      case Right(_) => false
    })
  }

  test("Parsing with spaces also") {
    assert(l.parse(" + - ! * +") match{
      case Left(v) => v == List(WS(0), Plus(1), WS(2), Minus(3), WS(4), Fact(5), WS(6), Multi(7), WS(8), Plus(9))
      case Right(_) => false
    })
  }

  test("Parsing with digits as well"){
    assert(l.parse("2+1") match{
      case Left(v) =>  v == List(Num(List('2'), 0), Plus(1), Num(List('1'), 2))
      case Right(_) => false
    })
  }

  test("Parsing with numbers as well"){
    assert(l.parse("19 ") match{
      case Left(v) =>  v == List(Num(List('1', '9'), 0), WS(2))
      case Right(_) => false
    })
    assert(l.parse("-222-+12") match{
      case Left(v) =>  v == List(Minus(0), Num(List('2', '2', '2'), 1), Minus(4), Plus(5), Num(List('1','2'), 6))
      case Right(_) => false
    })
  }

  test("Parsing unsigned floats") {
    assert(l.parse("1.") match{
      case Left(v) =>  v == List(Num(List('1', '.'), 0))
      case Right(_) => false
    })
    assert(l.parse("1.123") match {
      case Left(v) => v == List(Num(List('1', '.', '1', '2', '3'), 0))
      case Right(_) => false
    })
    assert(l.parse("1.23+1.56345345") match{
      case Left(v) =>
        v == List(Num(List('1', '.', '2', '3'),0), Plus(4), Num(List('1', '.', '5', '6', '3', '4', '5', '3', '4', '5'),5))
      case Right(_) => false
    })
  }

  test("Test with Exp") {
    assert(l.parse("1E11") match{
      case Left(v) =>  v == List(Num(List('1','E','1','1'),0))
      case Right(_) => false
    })
  }

  test("Test with Exp and frac") {
    assert(l.parse("1.4e1") match{
      case Left(v) =>  v == List(Num(List('1','.','4','e','1'),0))
      case Right(_) => false
    })
    assert(l.parse("021.6E+1++") match{
      case Left(v) =>  v == List(Num(List('0', '2', '1', '.','6','E', '+', '1'),0), Plus(8), Plus(9))
      case Right(_) => false
    })
    assert(l.parse("1.4E-1") match{
      case Left(v) =>  v == List(Num(List('1','.','4','E', '-', '1'),0))
      case Right(_) => false
    })
  }

  test("Test with cos") {
    assert(l.parse("cos1") match{
      case Left(v) =>  v == List(Cos(0), Num(List('1'), 3))
      case Right(_) => false
    })
    assert(l.parse("cOs!*1") match{
      case Left(v) =>  v == List(Cos(0), Fact(3), Multi(4), Num(List('1'), 5))
      case Right(_) => false
    })
  }

  test("Random") {
    assert(l.parse("cos+1.1E-1%%") match{
      case Left(_) => false
      case Right(v) =>  v == "Value at position 11 not expected"
    })
  }

}
