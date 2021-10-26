package io.github.cosminci.leetcode._400

object _338_CountingBits:

  def countBitsBottomUp(n: Int): Array[Int] =
    val dp = Array.ofDim[Int](n + 1)
    dp(0) = 0
    var offset = 1
    (1 to n).foreach { i =>
      if offset * 2 == i then offset = i
      dp(i) = 1 + dp(i - offset)
    }
    dp

  def countBitsBruteForce(n: Int): Array[Int] =
    val results = Array.ofDim[Int](n + 1)
    (0 to n).foreach { i =>
      var count = 0
      var num   = i
      while num != 0 do
        count += num % 2
        num = num / 2
      results(i) = count
    }
    results
