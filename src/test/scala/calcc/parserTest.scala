package calcc

/**
 * Created by joeisaacs on 18/08/15.
 */
import org.scalatest.{BeforeAndAfter, FunSuite}


class parserTest extends FunSuite with BeforeAndAfter {



  val table = new GrammarTable(ParserProductions.allProductions, ParserProductions.allGrammarSymbols)
  val t = table.grammarTable(ParserProductions.startItem)
  val p = new Parser(t)
  test("Parsing simple stuff") {
    //0
    assert(p.parse(List(LNum(List('0'), 0))) == Node(LNum(List('0'), 0), List(Leaf)))

    //0!
    assert(p.parse(List(LNum(List('0'), 0), LFact(0))) == Node(LFact(0), List(Node(LNum(List('0'), 0), List(Leaf)))))

    //Cos0
    assert(p.parse(List(LCos(0), LNum(List('0'),0))) == Node(LCos(0), List(Node(LNum(List('0'), 0), List(Leaf)))))

    //0+1
    assert(p.parse(List(LNum(List('0'),0), LPlus(0), LNum(List('1'),0))) ==
      Node(LPlus(0), List(Node(LNum(List('0'),0), List(Leaf)), Node(LNum(List('1'), 0), List(Leaf)))))

    //-1
    assert(p.parse(List(LMinus(0), LNum(List('0'),0))) ==
      Node(LMinus(0), List(Node(LNum(List('0'),0), List(Leaf)))))
  }

  test("Simple expressions") {
    assert(p.parse(List(LNum(List('0'),0), LMinus(0), LNum(List('1'),0))) ==
      Node(LMinus(0), List(Node(LNum(List('0'),0), List(Leaf)), Node(LNum(List('1'), 0), List(Leaf)))))

    assert(p.parse(List(LNum(List('0'),0), LMinus(0), LNum(List('1'),0))) ==
      Node(LMinus(0), List(Node(LNum(List('0'),0), List(Leaf)), Node(LNum(List('1'), 0), List(Leaf)))))
  }

  test("Precedence") {
    //0-1*1.1e-1
    assert(p.parse(List(LNum(List('0'), 0), LMinus(0), LNum(List('1'), 0), LMulti(0), LNum(List('1', '.', '1', 'E', '-', '1'), 0))) ==
      Node(LMinus(0),
        List(
          Node(LNum(List('0'), 0), List(Leaf)),
          Node(LMulti(0), List(
            Node(LNum(List('1'), 0), List(Leaf)),
            Node(LNum(List('1', '.', '1', 'E', '-', '1'), 0), List(Leaf))
          ))
        ))
    )
  }


  test("Complex") {
    //1!*cos2
    assert(p.parse(List(LNum(List('1'), 0), LFact(0), LMulti(0), LCos(0), LNum(List('2'), 0))) ==
      Node(
        LMulti(0),
        List(
          Node(LFact(0),
            List(
              Node(LNum(List('1'), 0), List(Leaf))
            )
          ),
          Node(LCos(0),
            List(
              Node(LNum(List('2'), 0), List(Leaf))
            ))
        )
      )
    )
  }
}
