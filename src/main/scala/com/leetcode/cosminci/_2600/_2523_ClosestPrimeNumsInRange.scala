package com.leetcode.cosminci._2600

import com.leetcode.cosminci.utils

object _2523_ClosestPrimeNumsInRange:

  def closestPrimes(left: Int, right: Int): Array[Int] =
    val sieve  = utils.sieveOfEratosthenes(right + 1)
    val primes = (left.max(2) to right).filter(n => sieve(n) == n)
    if primes.length < 2 then Array(-1, -1)
    else primes.sliding(2).minBy { case Seq(p1, p2) => p2 - p1 }.toArray
