package io.github.cosminci.leetcode._100

object _47_PermutationsII:
  def main(args: Array[String]): Unit =
    println(permuteUnique(Array(1, 2, 3)))

  def permuteUnique(input: Array[Int]): List[List[Int]] =
    def dfs(idx: Int): Set[List[Int]] =
      if idx == input.length - 1 then Set(List(input.last))
      else for
        permutation <- dfs(idx + 1)
        splitIdx    <- 0 to permutation.size
      yield
        val (left, right) = permutation.splitAt(splitIdx)
        left :+ input(idx) :++ right

    dfs(idx = 0).toList
