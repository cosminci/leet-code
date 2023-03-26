package com.leetcode.cosminci._2700

import com.leetcode.cosminci.utils

import scala.collection.immutable.TreeSet
import scala.util.chaining.*

object _2601_PrimeSubstractionOp:

  def primeSubOperation(nums: Array[Int]): Boolean =
    val primes = utils
      .sieveOfEratosthenes(1000)
      .zipWithIndex
      .collect { case (v, i) if i > 1 && v == i => v }
      .pipe(TreeSet.from)

    nums.foldLeft(0) { (prev, curr) =>
      if prev >= curr then return false
      primes.maxBefore(curr - prev) match
        case None               => curr
        case Some(biggestPrime) => curr - biggestPrime
    }.pipe(_ => true)
