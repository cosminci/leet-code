package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _20_ValidParantheses:

  def isValid(s: String): Boolean =
    @annotation.tailrec
    def dfs(stack: Seq[Char], idx: Int): Boolean =
      if idx == s.length then stack.isEmpty
      else if s(idx) == '(' then dfs(stack :+ ')', idx + 1)
      else if s(idx) == '[' then dfs(stack :+ ']', idx + 1)
      else if s(idx) == '{' then dfs(stack :+ '}', idx + 1)
      else if stack.isEmpty || s(idx) != stack.last then false
      else dfs(stack.dropRight(1), idx + 1)

    dfs(stack = Seq.empty, idx = 0)
