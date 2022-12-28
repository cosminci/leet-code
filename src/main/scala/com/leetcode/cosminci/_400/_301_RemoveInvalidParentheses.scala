package com.leetcode.cosminci._400

import scala.collection.mutable

object _301_RemoveInvalidParentheses:

  def removeInvalidParentheses(s: String): List[String] =
    val mem = mutable.Map.empty[(Int, Int), Set[String]]
    def dfs(i: Int, balance: Int): Set[String] = mem.getOrElseUpdate((i, balance),
      if balance < 0 then Set.empty
      else if i == s.length then if balance == 0 then Set("") else Set.empty
      else
        val skip       = if s(i) == ')' || s(i) == '(' then dfs(i + 1, balance) else Set.empty
        val newBalance = if s(i) == ')' then balance - 1 else if s(i) == '(' then balance + 1 else balance
        skip ++ dfs(i + 1, newBalance).map(suffix => s"${s(i)}$suffix")
    )

    val valid  = dfs(i = 0, balance = 0)
    val maxLen = valid.map(_.length).max
    valid.filter(_.length == maxLen).toList
