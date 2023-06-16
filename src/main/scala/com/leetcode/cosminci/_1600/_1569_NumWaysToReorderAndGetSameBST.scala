package com.leetcode.cosminci._1600

import scala.collection.mutable

object _1569_NumWaysToReorderAndGetSameBST:

  def numOfWays(nums: Array[Int]): Int =
    val mod = 1_000_000_007

    val mem = mutable.Map[(Int, Int), BigInt]((0, 0) -> 1)
    def combinations(k: Int, n: Int): BigInt = mem.getOrElseUpdate((k, n),
      if k == 0 || k == n then BigInt(1)
      else (combinations(k, n - 1) + combinations(k - 1, n - 1)) % mod
    )

    def numOfWays(nums: List[Int]): BigInt = nums match
      case Nil => 1
      case n :: ns =>
        val (lns, rns) = ns.partition(_ < n)
        Seq(numOfWays(lns), numOfWays(rns), combinations(lns.length, ns.length)).reduce((x, y) => (x * y) % mod)

    numOfWays(nums.toList).toInt - 1
