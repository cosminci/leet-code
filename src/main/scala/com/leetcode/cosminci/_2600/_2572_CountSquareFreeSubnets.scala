package com.leetcode.cosminci._2600

import scala.collection.mutable
import scala.util.chaining.*

object _2572_CountSquareFreeSubnets:

  def squareFreeSubsets(nums: Array[Int]): Int =
    val primes = Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)

    @annotation.tailrec
    def count(n: Int, prime: Int, cnt: Int = 0): Int =
      if cnt > 1 || n % prime != 0 then cnt
      else count(n / prime, prime, cnt + 1)

    def primeMask(n: Int): Int =
      primes.indices.foldLeft(0) { (mask, i) =>
        val cnt = count(n, primes(i))
        if cnt > 1 then return -1
        if cnt == 0 then mask else mask | (1 << (i + 1))
      }

    val mem = mutable.Map.empty[(Int, Int), Long]

    def dfs(i: Int, mask: Int): Long = mem.getOrElseUpdate((i, mask),
      if i == nums.length then 1L
      else
        val newMask = primeMask(nums(i))
        val skip    = dfs(i + 1, mask)
        val incl    = if (mask & newMask) == 0 then dfs(i + 1, newMask | mask) else 0
        (skip + incl) % 1_000_000_007
    )

    dfs(i = 0, mask = 1).toInt - 1
