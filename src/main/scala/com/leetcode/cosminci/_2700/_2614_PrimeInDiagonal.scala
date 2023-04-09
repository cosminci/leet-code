package com.leetcode.cosminci._2700

import com.leetcode.cosminci.utils.sieveOfEratosthenes

object _2614_PrimeInDiagonal:

  def diagonalPrime(nums: Array[Array[Int]]): Int =
    val sieve           = sieveOfEratosthenes(4_000_001)
    def isPrime(n: Int) = n >= 2 && sieve(n) == n

    val max1 = nums.indices.map(i => nums(i)(i)).filter(isPrime).maxOption.getOrElse(0)
    val max2 = nums.indices.map(i => nums(i)(nums.length - 1 - i)).filter(isPrime).maxOption.getOrElse(0)

    max1.max(max2)
