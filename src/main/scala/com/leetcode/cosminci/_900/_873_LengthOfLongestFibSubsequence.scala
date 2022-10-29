package com.leetcode.cosminci._900

import scala.collection.mutable

object _873_LengthOfLongestFibSubsequence:
  def main(args: Array[String]): Unit =
    println(lenLongestFibSubseqSet(Array(1, 2, 3, 4, 5, 6, 7, 8)))
    println(lenLongestFibSubseqDP(Array(1, 2, 3, 4, 5, 6, 7, 8)))
    println(lenLongestFibSubseqSet(Array(1, 3, 7, 11, 12, 14, 18)))
    println(lenLongestFibSubseqDP(Array(1, 3, 7, 11, 12, 14, 18)))

  def lenLongestFibSubseqSet(arr: Array[Int]): Int =
    val distinct = Set.from(arr)
    def longest(prev2: Int, prev1: Int): Int =
      Option.when(distinct.contains(prev2 + prev1))(1 + longest(prev1, prev2 + prev1)).getOrElse(0)

    val results = for
      i <- 0 until arr.length - 2
      j <- i + 1 until arr.length - 1
    yield longest(arr(i), arr(j)) + 2

    results.filter(_ > 0).maxOption.getOrElse(0)

  def lenLongestFibSubseqDP(arr: Array[Int]): Int =
    val dp       = mutable.Map.empty[(Int, Int), Int].withDefaultValue(2)
    val distinct = Set.from(arr)

    for
      j <- 2 until arr.length
      i <- 1 until j
      if arr(j) - arr(i) < arr(i) && distinct.contains(arr(j) - arr(i))
    do dp.update((arr(i), arr(j)), dp((arr(j) - arr(i), arr(i))) + 1)

    dp.values.maxOption.getOrElse(0)
