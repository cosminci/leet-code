package com.leetcode.cosminci._100

object _32_LongestValidParantheses:

  def longestValidParentheses(s: String): Int =
    s.indices
      .foldLeft(Array(-1), 0) { case ((stack, result), i) =>
        if s.charAt(i) == ')' && stack.length > 1 && s(stack.last) == '(' then
          (stack.dropRight(1), result.max(i - stack.dropRight(1).last))
        else (stack :+ i, result)
      }._2
