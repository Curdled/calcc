/**
 * Created by joeisaacs on 18/08/15.
 */

import calcc.Parser
import calcc._

import sext._




object Main {
  def main(args: Array[String]) {
    val l = new Lexer(LexerDFA.tranisitions, LexerDFA.start, LexerDFA.finish)
    val input = l.lex(args.head)
    input match{
      case Left(s) =>
        val table = new GrammarTable(ParserProductions.allProductions, ParserProductions.allGrammarSymbols)
        val t = table.grammarTable(ParserProductions.startItem)
        val p = new Parser(t)
        val output = p.parse(s)
        println(output.treeString)
      case Right(s) => throw new IllegalArgumentException(s)
    }


  }
}