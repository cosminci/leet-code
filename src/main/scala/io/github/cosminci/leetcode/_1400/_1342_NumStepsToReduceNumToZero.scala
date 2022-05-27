package io.github.cosminci.leetcode._1400

object _1342_NumStepsToReduceNumToZero:

  def numberOfSteps(num: Int): Int =
    @annotation.tailrec
    def dfs(num: Int, steps: Int): Int =
      if num == 0 then steps
      else if num % 2 == 0 then dfs(num / 2, steps + 1)
      else dfs(num - 1, steps + 1)

    dfs(num, steps = 0)
