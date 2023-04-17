package com.leetcode.cosminci._1500

object _1431_KidsWithGreatestNumCandies:

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): List[Boolean] =
    val max = candies.max
    candies.map(_ + extraCandies >= max).toList
