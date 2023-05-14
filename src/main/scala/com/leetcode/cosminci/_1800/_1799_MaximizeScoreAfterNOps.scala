package com.leetcode.cosminci._1800

import com.leetcode.cosminci.utils.gcd
import scala.collection.mutable

object _1799_MaximizeScoreAfterNOps:

  def maxScore(nums: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(i: Int, mask: Int): Int = mem.getOrElseUpdate((i, mask),
      if i > nums.length / 2 then 0
      else nums.indices.foldLeft(0) { (res, j) =>
        (j + 1 until nums.length).foldLeft(res) { (res, k) =>
          val newMask = (1 << j) + (1 << k)
          if (mask & newMask) != 0 then res
          else res.max(i * gcd(nums(j), nums(k)) + dfs(i + 1, mask + newMask))
        }
      }
    )
    dfs(i = 1, mask = 0)
