package io.github.cosminci.leetcode._100

object _32_LongestValidParantheses:
  def main(args: Array[String]): Unit =
    println((longestValidParentheses("(()")))
    println((longestValidParentheses(")()())")))
    println((longestValidParentheses("")))

  def longestValidParentheses(s: String): Int =
    var max = 0

    var (l, r) = (0, 0)
    s.indices.foreach { i =>
      if s(i) == '(' then l += 1 else r += 1
      if l == r then max = math.max(max, r * 2)
      else if r > l then
        l = 0; r = 0
    }

    l = 0; r = 0
    s.indices.reverse.foreach { i =>
      if s(i) == '(' then l += 1 else r += 1
      if l == r then max = math.max(max, l * 2)
      else if l > r then
        l = 0; r = 0
    }
    max
