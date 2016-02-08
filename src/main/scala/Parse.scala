import data.ParseTree
import data.ParseTree.Cons
import data.ParseTree.Lit
import data.ParseTree.Pair
import data.ParseTree.Left
import data.ParseTree.Right
import data.ParseTree.Void
import data.Regex
import data.Regex.Alt
import data.Regex.Con
import data.Regex.Epsilon
import data.Regex.Let
import data.Regex.Star
import data.Regex.Var
import data.Word
import scala.util.Try

object Parse {
  private def makeEps(r: Regex): ParseTree = r match {
    case Star(_) => ParseTree.Nil
    case Con(r1, r2) => Pair(makeEps(r1), makeEps(r2))
    case Alt(r1, r2) =>
      if (Helper.canEmpty(r1))
        Left(makeEps(r1))
      else if (Helper.canEmpty(r2))
        Right(makeEps(r2))
      else
        throw new RuntimeException("error of the alternation in makeEps")
    case Epsilon => Void
    case e => throw new RuntimeException(s"error in makeEps: $e")
  }

  private def inject(r: Regex, l: Char, t: ParseTree): ParseTree = (r, t) match {
    case (Var(_, r), t) => inject(r, l, t)
    case (Star(r), Pair(v, vs)) => Cons(inject(r, l, v), vs)
    case (Con(r1, r2), t) => t match {
      case Pair(v1, v2) => Pair(inject(r1, l, v1), v2)
      case Left(Pair(v1, v2)) => Pair(inject(r1, l, v1), v2)
      case Right(v2) => Pair(makeEps(r1), inject(r2, l, v2))
      case e => throw new RuntimeException(s"error of the concatenation in inject: $e")
    }
    case (Alt(r1, r2), t) => t match {
      case Left(v1) => Left(inject(r1, l, v1))
      case Right(v2) => Right(inject(r2, l, v2))
      case e => throw new RuntimeException(s"error of the alternation in inject: $e")
    }
    case (Let(l), Void) => Lit(l)
    case e => throw new RuntimeException(s"error in inject: $e")
  }

  def parse(r: Regex, str: Word): Try[ParseTree] =
    str match {
      case Nil if Helper.canEmpty(r) => Try(makeEps(r))
      case l::w => parse(Derivative.derivative(r, l), w).map(t => inject(r, l, t))
      case e => Try(throw new RuntimeException(s"error in parse: $e"))
    }
}
