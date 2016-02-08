import data.ParseTree
import data.ParseTree.Cons
import data.ParseTree.Left
import data.ParseTree.Lit
import data.ParseTree.Nil
import data.ParseTree.Pair
import data.ParseTree.Right
import data.ParseTree.Void
import data.Regex
import data.Regex.Alt
import data.Regex.Con
import data.Regex.Empty
import data.Regex.Epsilon
import data.Regex.Let
import data.Regex.Star
import data.Regex.Var

object Helper {
  def canEmpty(r: Regex): Boolean = r match {
    case Epsilon => true
    case Let(_) => false
    case Alt(r1, r2) => canEmpty(r1) || canEmpty(r2)
    case Con(r1, r2) => canEmpty(r1) && canEmpty(r2)
    case Star(r) => true
    case Empty => false
    case Var(_, r) => canEmpty(r)
  }

  def pp(t: ParseTree): String = t match {
    case Void => "()"
    case Nil => "[]"
    case Lit(l) => s"$l"
    case Pair(t1, t2) => s"(${pp(t1)}, ${pp(t2)})"
    case Left(t) => s"Left(${pp(t)})"
    case Right(t) => s"Right(${pp(t)})"
    case Cons(v, vs) => s"${pp(v)}: ${pp(vs)}"
  }
}
