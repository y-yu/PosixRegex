import data.Regex
import data.Regex.Alt
import data.Regex.Con
import data.Regex.Empty
import data.Regex.Epsilon
import data.Regex.Let
import data.Regex.Star
import data.Regex.Var

object Derivative {
  def derivative(r: Regex, l: Char): Regex = r match {
    case Empty => Empty
    case Epsilon => Empty
    case Let(c) => if (c == l) Epsilon else Empty
    case Alt(r1, r2) => Alt(derivative(r1, l), derivative(r2, l))
    case Con(r1, r2) =>
      if (Helper.canEmpty(r1))
        Alt(Con(derivative(r1, l), r2), derivative(r2, l))
      else
        Con(derivative(r1, l), r2)
    case Star(r) => Con(derivative(r, l), Star(r))
    case Var(_, r) => derivative(r, l)
  }
}
