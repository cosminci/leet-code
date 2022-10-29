package com.leetcode.cosminci._2200

import com.leetcode.cosminci.utils.gcd

object _2183_CountArrayPairsDivisibleByK:

  def countPairs(nums: Array[Int], k: Int): Long =
    nums
      .foldLeft(Map.empty[Int, Int].withDefaultValue(0), 0L) { case ((gcds, result), n) =>
        val currGcd  = gcd(n, k)
        val nextGcds = gcds.updated(currGcd, gcds(currGcd) + 1)
        val nextResult = gcds.foldLeft(result) { case (result, (prevGcd, count)) =>
          if currGcd.toLong * prevGcd % k == 0 then result + count else result
        }
        (nextGcds, nextResult)
      }._2
