object Main {
  def main(args: Array[String]): Unit = {
    val regex = RegexParser.parse(args(0))
    println(regex)

    val tree = regex.flatMap(Parse.parse(_, args(1).toCharArray.toList))
    println(tree.map(Helper.pp))

    for {
      r <- regex
      t <- tree
    } yield println(Submatch.submatch(t, r))
  }
}
