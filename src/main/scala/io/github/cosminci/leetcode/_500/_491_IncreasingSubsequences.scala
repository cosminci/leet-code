package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _491_IncreasingSubsequences:
  def main(args: Array[String]): Unit =
    println(findSubsequences(Array(4, 6, 7, 7)))

  private def findSubsequences(nums: Array[Int]): List[List[Int]] =
    if nums.length < 2 then return List.empty

    val mem = mutable.Map.empty[(Int, Int), Seq[Seq[Int]]]
    def dfs(idx: Int, prevValue: Int): Seq[Seq[Int]] =
      if idx == nums.length then return Seq(Seq.empty)
      if mem.contains(idx, prevValue) then return mem((idx, prevValue))

      val result =
        if nums(idx) >= prevValue then
          dfs(idx + 1, nums(idx)).map(_.prepended(nums(idx))) ++ dfs(idx + 1, prevValue)
        else dfs(idx + 1, prevValue)

      mem.update((idx, prevValue), result)
      result

    val subsequences = for
      i <- 0 until nums.length - 1
      j <- i + 1 until nums.length
      if nums(i) <= nums(j)
    yield dfs(j + 1, nums(j)).map(Seq(nums(i), nums(j)) ++ _)

    subsequences.flatten.distinct.map(_.toList).toList
