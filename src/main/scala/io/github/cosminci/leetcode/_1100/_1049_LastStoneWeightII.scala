package io.github.cosminci.leetcode._1100

import scala.collection.mutable

object _1049_LastStoneWeightII:
  def main(args: Array[String]): Unit =
    println(lastStoneWeightIITopDown(Array(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 14, 23, 37, 61, 98)))
    println(lastStoneWeightIIBottomUp(Array(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 14, 23, 37, 61, 98)))

  def lastStoneWeightIITopDown(stones: Array[Int]): Int =
    val mem = mutable.Map.empty[(Int, Int, Int), Int]

    def dfs(idx: Int, bucket1: Int, bucket2: Int): Int =
      if idx == stones.length then return math.abs(bucket1 - bucket2)
      if mem.contains((idx, bucket1, bucket2)) then return mem((idx, bucket1, bucket2))

      val result = math.min(dfs(idx + 1, bucket1 + stones(idx), bucket2), dfs(idx + 1, bucket1, bucket2 + stones(idx)))
      mem.update((idx, bucket1, bucket2), result)

      result

    dfs(idx = 0, bucket1 = 0, bucket2 = 0)

  /* Equivalent to finding the subsequence sum closest to total sum / 2 */
  def lastStoneWeightIIBottomUp(stones: Array[Int]): Int =
    val totalSum     = stones.sum
    val possibleSums = mutable.Set(0)
    stones.foreach { stone =>
      val newPossibleSums = possibleSums.collect {
        case s if s + stone <= totalSum / 2 =>
          s + stone
      }
      possibleSums.addAll(newPossibleSums)
    }
    totalSum - 2 * possibleSums.max
