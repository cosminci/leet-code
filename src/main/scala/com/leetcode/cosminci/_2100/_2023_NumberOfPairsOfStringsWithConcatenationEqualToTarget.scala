package com.leetcode.cosminci._2100

object _2023_NumberOfPairsOfStringsWithConcatenationEqualToTarget:
  def main(args: Array[String]): Unit =
    println(numOfPairsBruteForce(Array("777", "7", "77", "77"), "7777"))
    println(numOfPairsFreqMap(Array("777", "7", "77", "77"), "7777"))
    println(numOfPairsBruteForce(Array("1201954", "543", "3", "12019"), "12019543"))
    println(numOfPairsFreqMap(Array("1201954", "543", "3", "12019"), "12019543"))

  def numOfPairsBruteForce(nums: Array[String], target: String): Int =
    val hits = for
      i <- nums.indices
      j <- nums.indices
      if j != i && s"${nums(i)}${nums(j)}" == target
    yield true
    hits.length

  def numOfPairsFreqMap(nums: Array[String], target: String): Int =
    val freqMap  = Map.from(nums.groupBy(identity).mapValues(_.length)).withDefaultValue(0)
    val prefixes = target.scanLeft("")(_ + _).slice(1, target.length)

    prefixes.foldLeft(0) { case (result, prefix) =>
      val suffix     = target.stripPrefix(prefix)
      val correction = if prefix == suffix then 1 else 0
      result + freqMap(prefix) * (freqMap(suffix) - correction)
    }
