package com.leetcode.cosminci._2300

object _2211_CountColissionsOnARoad:
  def countCollisions(directions: String): Int =
    directions
      .dropWhile(_ == 'L')
      .reverse
      .dropWhile(_ == 'R')
      .count(_ != 'S')
