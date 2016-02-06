package data

sealed trait ParseTree
object ParseTree {
  case object Void extends ParseTree
  case object Nil extends ParseTree
  case class Lit(l: Char) extends ParseTree
  case class Pair(t1: ParseTree, t2: ParseTree) extends ParseTree
  case class Left(t: ParseTree) extends ParseTree
  case class Right(t: ParseTree) extends ParseTree
  case class Cons(t: ParseTree, ts: ParseTree) extends ParseTree
}
