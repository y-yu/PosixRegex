import data.ParseTree
import data.ParseTree.Cons
import data.ParseTree.Lit
import data.ParseTree.Void
import data.ParseTree.Nil
import data.ParseTree.Right
import data.ParseTree.Left
import data.ParseTree.Pair
import data.Regex
import data.Regex.Alt
import data.Regex.Con
import data.Regex.Epsilon
import data.Regex.Let
import data.Regex.Star
import data.Regex.Var
import data.Word

object Submatch {
  private def flatten(t: ParseTree): Word = t match {
    case Void | Nil => List()
    case Left(v) => flatten(v)
    case Right(v) => flatten(v)
    case Cons(v, vs) => flatten(v) ++ flatten(vs)
    case Pair(v1, v2) => flatten(v1) ++ flatten(v2)
    case Lit(l) => List(l)
  }

  def submatch(t: ParseTree, r: Regex): Set[Map[String, Word]] = (t, r) match {
    case (Void, Epsilon) => Set.empty
    case (Lit(l), Let(c)) => if (l == c) Set.empty else throw new RuntimeException("error")
    case (Nil, Star(_)) => Set.empty
    case (Cons(v, vs), Star(r)) => submatch(v, r) ++ submatch(vs, Star(r))
    case (Pair(v1, v2), Con(r1, r2)) => submatch(v1, r1) ++ submatch(v2, r2)
    case (Left(v), Alt(r1, _)) => submatch(v, r1)
    case (Right(v), Alt(_, r2)) => submatch(v, r2)
    case (v, Var(n, r)) => submatch(v, r) ++ Set(Map(n -> flatten(v)))
    case e => throw new RuntimeException(s"error in submatch: $e")
  }
}
