import java.io.StringReader
import data.Regex
import data.Regex.Alt
import data.Regex.Con
import data.Regex.Empty
import data.Regex.Let
import data.Regex.Star
import data.Regex.Var
import scala.util.Try
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.StreamReader

// 参考: https://github.com/kmizu/minimal_regex_matcher/blob/master/src/main/scala/com/github/kmizu/minimal_regex_matcher/RegexParser.scala
object RegexParser extends Parsers {
  type Elem = Char

  lazy val BAR = chr('|')
  lazy val QUESTION = chr('?')
  lazy val STAR = chr('*')
  lazy val PLUS = chr('+')
  lazy val OPEN = chr('(')
  lazy val CLOSE = chr(')')
  lazy val BACKSLASH = chr('\\')
  lazy val COLON = chr(':')

  lazy val END_OF_FILE = not(any)

  private val any: Parser[Char] = elem(".", c => c != CharSequenceReader.EofCh)

  private def chr(c: Char): Parser[Char] = c

  lazy val Regex: Parser[Regex] = Expression <~ END_OF_FILE

  lazy val Expression: Parser[Regex] = rep1sep(Sequence, BAR) ^^ { ns =>
    ns.tail.foldLeft(ns.head) { (a, y) => Alt(a, y)}
  }

  lazy val Sequence: Parser[Regex] = Suffix.+ ^^ { ns =>
    ns.tail.foldLeft(ns.head) { (a, y) => Con(a, y)}
  }

  lazy val Suffix: Parser[Regex] = (
    Primary <~ QUESTION ^^ { case e => Alt(e, Empty)}
      | Primary <~ STAR ^^ { case e => Star(e)}
      | Primary <~ PLUS ^^ { case e => Con(e, Star(e))}
      | Primary
  )

  lazy val Variable: Parser[Regex] = CHAR.+ ~ COLON ~ Expression ^^ {
    case (n ~ _ ~ r) => Var(n.foldLeft("")((a, y) => a + y), r)
  }

  lazy val Primary: Parser[Regex] = (
    Escape
      | OPEN ~> Variable <~ CLOSE
      | OPEN ~> Expression <~ CLOSE
      | Literal
  )

  lazy val Escape: Parser[Regex] = BACKSLASH ~> (BAR | QUESTION | STAR | OPEN | CLOSE | COLON | BACKSLASH) ^^ { e => Let(e) }

  lazy val Literal: Parser[Regex] = CHAR ^^ {
    case c => Let(c)
  }

  lazy val CHAR: Parser[Char] = not(BAR | QUESTION | STAR | OPEN | CLOSE | BACKSLASH | COLON) ~> any

  def parse(input: String): Try[Regex] = Try(
    Regex(StreamReader(new StringReader(input))) match {
      case Success(node, _) => node
      case Failure(msg, rest) =>
        throw new RuntimeException(msg)
      case Error(msg, rest) =>
        throw new RuntimeException(msg)
    }
  )
}
