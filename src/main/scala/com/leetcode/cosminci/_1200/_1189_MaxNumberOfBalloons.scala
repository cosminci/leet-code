package com.leetcode.cosminci._1200

import com.leetcode.cosminci.utils

object _1189_MaxNumberOfBalloons:
  def main(args: Array[String]): Unit =
    println(maxNumberOfBalloons("nlaebolko"))
    println(maxNumberOfBalloons("loonbalxballpoon"))

  def maxNumberOfBalloons(text: String): Int =
    val letterCounts = utils.characterCounts(text)
    Map('b' -> 1, 'a' -> 1, 'l' -> 2, 'o' -> 2, 'n' -> 1).map { case (char, weight) =>
      letterCounts(char - 'a') / weight
    }.min
