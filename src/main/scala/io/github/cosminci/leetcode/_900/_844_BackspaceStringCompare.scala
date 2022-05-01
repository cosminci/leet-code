package io.github.cosminci.leetcode._900

object _844_BackspaceStringCompare:
  def main(args: Array[String]): Unit =
    println(backspaceCompare("y#fo##f", "y#f#o##f"))

  def backspaceCompare(s: String, t: String): Boolean =
    @annotation.tailrec
    def dfs(s: String, stack: Array[Char] = Array.empty, i: Int = 0): String =
      if i == s.length then stack.mkString
      else if s(i) == '#' then dfs(s, stack.dropRight(Option.when(s(i) == '#')(1).getOrElse(0)), i + 1)
      else dfs(s, stack :+ s(i), i + 1)

    dfs(s) == dfs(t)
