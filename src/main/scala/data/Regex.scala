package data

sealed trait Regex
object Regex {
  case class Let(l: Char) extends Regex
  case class Alt(r1: Regex, r2: Regex) extends Regex
  case class Star(r: Regex) extends Regex
  case class Con(r1: Regex, r2: Regex) extends Regex
  case object Epsilon extends Regex
  case object Empty extends Regex
}
