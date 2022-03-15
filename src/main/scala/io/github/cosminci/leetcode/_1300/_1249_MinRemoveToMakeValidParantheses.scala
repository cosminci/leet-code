package io.github.cosminci.leetcode._1300

import scala.collection.mutable

object _1249_MinRemoveToMakeValidParantheses:
  def main(args: Array[String]): Unit =
    println(minRemoveToMakeValid("lee(t(c)o)de)"))
    println(minRemoveToMakeValid("a)b(c)d"))
    println(minRemoveToMakeValid("))(("))
    println(minRemoveToMakeValid("(a(b(c)d)"))

  def minRemoveToMakeValid(s: String): String =
    @annotation.tailrec
    def dfs(stack: Array[(Char, Int)], i: Int): Array[(Char, Int)] =
      if i == s.length then stack
      else if s(i) == ')' && stack.lastOption.exists(_._1 == '(') then dfs(stack.dropRight(1), i + 1)
      else if s(i) == ')' then dfs(stack :+ (')', i), i + 1)
      else if s(i) == '(' then dfs(stack :+ ('(', i), i + 1)
      else dfs(stack, i + 1)

    val toRemove = dfs(stack = Array.empty, i = 0).map(_._2).toSet
    s.indices.filterNot(toRemove.contains).map(s).mkString
