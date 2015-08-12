package calcc

import org.scalatest.{BeforeAndAfter, FunSuite}

import scala.collection.convert.Wrappers.MutableBufferWrapper

/**
 * Created by joe on 11/08/15.
 */
class lexerTest extends FunSuite with BeforeAndAfter {

  val l = new Lexer(LexerDFA.tranisitions, LexerDFA.start, LexerDFA.finish)


  test("Parsing + - ! *") {
    assert(l.parse("+") == Left(List(LPlus(0))))

    assert(l.parse("+-!*++") == Left(List(LPlus(0), LMinus(1), LFact(2), LMulti(3), LPlus(4), LPlus(5))))
  }

  test("Parsing with spaces also") {
    assert(l.parse(" + - ! * +") ==
      Left(List(LWS(0), LPlus(1), LWS(2), LMinus(3), LWS(4), LFact(5), LWS(6), LMulti(7), LWS(8), LPlus(9))))
  }

  test("Parsing with digits as well"){
    assert(l.parse("2+1") == Left(List(LNum(List('2'), 0), LPlus(1), LNum(List('1'), 2))))
  }

  test("Parsing with numbers as well"){
    assert(l.parse("19 ") == Left(List(LNum(List('1', '9'), 0), LWS(2))))
    assert(l.parse("-222-+12") == Left(List(LMinus(0), LNum(List('2', '2', '2'), 1), LMinus(4), LPlus(5), LNum(List('1','2'), 6))))
  }

  test("Parsing unsigned floats") {
    assert(l.parse("1.") == Left(List(LNum(List('1', '.'), 0))))

    assert(l.parse("1.123") == Left(List(LNum(List('1', '.', '1', '2', '3'), 0))))

    assert(l.parse("1.23+1.56345345") ==
      Left(List(LNum(List('1', '.', '2', '3'),0), LPlus(4), LNum(List('1', '.', '5', '6', '3', '4', '5', '3', '4', '5'),5))))
  }

  test("Test with Exp") {
    assert(l.parse("1E11".toCharArray.toList) == Left(List(LNum(List('1','E','1','1'),0))))
  }

  test("Test with Exp and frac") {
    assert(l.parse("1.4e1") == Left(List(LNum(List('1','.','4','e','1'),0))))

    assert(l.parse("021.6E+1++")  == Left(List(LNum(List('0', '2', '1', '.','6','E', '+', '1'),0), LPlus(8), LPlus(9))))

    assert(l.parse("1.4E-1") == Left(List(LNum(List('1','.','4','E', '-', '1'),0))))
  }

  test("Test with cos") {
    assert(l.parse("cos1") == Left(List(LCos(0), LNum(List('1'), 3))))
    assert(l.parse("cOs!*1") == Left(List(LCos(0), LFact(3), LMulti(4), LNum(List('1'), 5))))
  }

  test("Random") {
    assert(l.parse("cos+1.1E-1%%") == Right("Value at position 11 not expected"))
  }
}