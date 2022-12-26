package com.leetcode.cosminci._2600

object _2518_NumGreatPartitions:

  def countPartitions(nums: Array[Int], k: Int): Int =
    val mod = 1_000_000_007

    val dp  = Array.tabulate(k)(i => if i == 0 then 1L else 0L)
    nums.foreach { num =>
      (k - 1 - num to 0 by -1).foreach { i =>
        dp(i + num) = (dp(i + num) + dp(i)) % mod
      }
    }

    val sum     = nums.map(_.toLong).sum
    val total   = Iterator.iterate(1L)(v => (v * 2) % mod).drop(nums.length).next()
    val invalid = (0 until k).map(i => if sum - i < k then dp(i) else dp(i) * 2).sum

    math.floorMod((total - invalid) % mod, mod).toInt
