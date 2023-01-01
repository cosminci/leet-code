package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2521_DistinctPrimeFactorsOfArrayProduct:
  private val smallPrimes = Seq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31)

  def distinctPrimeFactors(nums: Array[Int]): Int =
    nums.foldLeft(Set.empty[Int]) { (primes, n) =>
      smallPrimes.foldLeft(primes, n) { case ((primes, n), sp) =>
          val nextPrimes = if n % sp == 0 then primes + sp else primes
          val nextN      = Iterator.iterate(n)(_ / sp).dropWhile(_ % sp == 0).next
          (nextPrimes, nextN)
      }.pipe { case (primes, n) => if n != 1 then primes + n else primes }
    }.size
