import calcc.Parser
import calcc._


object Main {
  def main(args: Array[String]) {
    val l = new Lexer(LexerDFA.tranisitions, LexerDFA.start, LexerDFA.finish)
    val input = l.lex(args.head)
    input match{
      case Left(s) =>
        val table = new GrammarTable(ParserProductions.allProductions, ParserProductions.allGrammarSymbols)
        val t = table.grammarTable(ParserProductions.startItem)
        val p = new Parser(t)
        p.parse(s) match{
          case Left(output) => println(output)
          case Right(error) => System.err.println(error)
        }

      case Right(s) => throw new IllegalArgumentException(s)
    }
  }
}