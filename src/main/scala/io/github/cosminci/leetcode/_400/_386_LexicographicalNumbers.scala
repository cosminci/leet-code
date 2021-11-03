package io.github.cosminci.leetcode._400

object _386_LexicographicalNumbers:
  def main(args: Array[String]): Unit =
    println(lexicalOrder(101))

  def lexicalOrder(max: Int): List[Int] =
    def dfs(n: Int): Seq[Int] =
      if n > max then Seq.empty
      else n +: (0 to 9).flatMap(i => dfs(10 * n + i))

    (1 to 9).flatMap(dfs).toList
