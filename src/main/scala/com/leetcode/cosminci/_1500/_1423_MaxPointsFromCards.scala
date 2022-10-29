package com.leetcode.cosminci._1500

object _1423_MaxPointsFromCards:

  def maxScore(cards: Array[Int], k: Int): Int = {
    val n = cards.length

    val windowSums = (n - k until n)
      .scanLeft(cards.take(n - k).sum)((prevSum, i) => prevSum - cards(i - n + k) + cards(i))

    cards.sum - windowSums.min
  }
