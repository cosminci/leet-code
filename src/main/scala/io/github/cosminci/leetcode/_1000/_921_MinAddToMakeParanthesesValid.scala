package io.github.cosminci.leetcode._1000

object _921_MinAddToMakeParanthesesValid:
  def minAddToMakeValid(s: String): Int =
    val (result, balance) = s.foldLeft(0, 0) { case ((total, balance), p) =>
      p match
        case '(' => (total, balance + 1)
        case ')' => if balance == 0 then (total + 1, 0) else (total, balance - 1)
    }
    result + balance
