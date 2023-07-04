package com.leetcode.cosminci._2800

import com.leetcode.cosminci.utils.sieveOfEratosthenes

object _2761_PrimePairsWithTargetSum:

  def findPrimePairs(n: Int): List[List[Int]] =
    val primes = sieveOfEratosthenes(n)
    (2 to n / 2)
      .collect { case p if primes(p) == p && primes(n - p) == n - p => List(p, n - p) }
      .toList
