package com.leetcode.cosminci._1000

object _946_ValidateStackSequences:
  def validateStackSequences(pushed: Array[Int], popped: Array[Int]): Boolean =
    @annotation.tailrec
    def dfs(stack: Seq[Int], idx: Int): (Seq[Int], Int) =
      if (stack.headOption.forall(_ != popped(idx))) (stack, idx)
      else dfs(stack.tail, idx + 1)

    pushed.foldLeft(Seq.empty[Int], 0) {
      case ((stack, idx), n) =>
        dfs(n +: stack, idx)
    }._1.isEmpty
