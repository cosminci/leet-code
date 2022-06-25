package io.github.cosminci.leetcode._2400

object _2315_CountAsterisks:

  def countAsterisks(s: String): Int =
    s.foldLeft(0, false) { case ((count, betweenBars), char) =>
      if char == '*' && !betweenBars then (count + 1, betweenBars ^ char == '|')
      else (count, betweenBars ^ char == '|')
    }._1
