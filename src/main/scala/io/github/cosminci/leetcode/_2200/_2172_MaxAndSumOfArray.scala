package io.github.cosminci.leetcode._2200

import scala.collection.mutable

object _2172_MaxAndSumOfArray:
  def main(args: Array[String]): Unit =
    println(maximumANDSum(Array(14, 7, 9, 8, 2, 4, 11, 1, 9), 8))

  def maximumANDSum(nums: Array[Int], numSlots: Int): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(idx: Int, mask: Int): Int = mem.getOrElseUpdate((idx, mask), {
      if idx == nums.length then 0
      else (0 until numSlots).flatMap { i =>
        Seq(0, 1).collectFirst {
          case posInSlot if (mask & (1 << (2 * i + posInSlot))) == 0 =>
            val newMask = mask | (1 << (2 * i + posInSlot))
            (nums(idx) & (i + 1)) + dfs(idx + 1, newMask)
        }
      }.max
    })

    dfs(idx = 0, mask = 0)
