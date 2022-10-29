package com.leetcode.cosminci._200

object _135_Candy:

  def candy(ratings: Array[Int]): Int =
    val leftToRight = (1 until ratings.length).scanLeft(1) { (prevCandies, i) =>
      if ratings(i) > ratings(i - 1) then prevCandies + 1 else 1
    }
    val rightToLeft = (0 until ratings.length - 1).scanRight(leftToRight.last) { (i, nextCandies) =>
      if ratings(i) > ratings(i + 1) then (1 + nextCandies).max(leftToRight(i)) else leftToRight(i)
    }
    rightToLeft.sum
