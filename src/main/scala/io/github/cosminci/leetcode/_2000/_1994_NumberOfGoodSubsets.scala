package io.github.cosminci.leetcode._2000

import scala.collection.mutable

object _1994_NumberOfGoodSubsets:
  def main(args: Array[String]): Unit =
    println(numberOfGoodSubsets(Array(1, 2, 3, 4)))

  def numberOfGoodSubsets(nums: Array[Int]): Int =
    val mod    = 1_000_000_007
    val primes = Seq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
    val dp     = Array.tabulate[Long](1 << primes.length + 1)(i => if i == 0 then 1 else 0)

    val numCounts = nums.groupBy(identity).view.mapValues(_.length).toMap
    numCounts.foreach { case (num, count) =>
      if num != 1 && num % 4 != 0 && num % 9 != 0 && num % 25 != 0 then
        val primeFactors = primes.zipWithIndex.collect { case (p, i) if num % p == 0 => 1 << i }.sum

        dp.indices.foreach { possiblePrimeFactors =>
          if (possiblePrimeFactors & primeFactors) == 0 then
            val resultingPrimeFactors = possiblePrimeFactors | primeFactors
            dp(resultingPrimeFactors) += (numCounts(num) * dp(possiblePrimeFactors)) % mod
        }
    }

    val oneFactor = (math.pow(2, numCounts.getOrElse(1, 0)) % mod).toInt
    val dpSum     = dp.foldLeft(0L) { case (acc, v) => (acc + v) % mod } - 1
    ((oneFactor * dpSum) % mod).toInt
