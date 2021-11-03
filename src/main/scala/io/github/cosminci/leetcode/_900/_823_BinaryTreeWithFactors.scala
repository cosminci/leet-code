package io.github.cosminci.leetcode._900

import scala.collection.mutable

object _823_BinaryTreeWithFactors:
  def main(args: Array[String]): Unit =
    println(numFactoredBinaryTrees(Array(2, 4)))
    println(numFactoredBinaryTrees(Array(2, 4, 5, 10)))

  def numFactoredBinaryTrees(arr: Array[Int]): Int =
    (arr.sorted.foldLeft(Map.empty[Int, Long].withDefaultValue(0L)) { (dp, a) =>
      dp.updated(a, dp.keys.foldLeft(1L) { (count, b) =>
        count + Option.when(a % b == 0)(dp(b) * dp(a / b)).getOrElse(0L)
      })
    }.values.sum % 1_000_000_007).toInt
