package io.github.cosminci.leetcode._2500

object _2411_SmallestSubarraysWithMaxBitwiseOr:

  def smallestSubarrays(nums: Array[Int]): Array[Int] =
    val lastBitPos = nums.indices.scanRight(Seq.fill(30)(0)) { (i, lastBitPos) =>
      lastBitPos.indices.map { bit =>
        if (nums(i) & 1 << bit) > 0 then i else lastBitPos(bit)
      }
    }
    nums.indices.map { i =>
      (0 until 30)
        .map(bit => lastBitPos(i)(bit) - i + 1)
        .max
        .max(1)
    }.toArray
