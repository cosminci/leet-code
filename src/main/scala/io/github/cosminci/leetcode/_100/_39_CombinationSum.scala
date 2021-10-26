package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _39_CombinationSum:
  def main(args: Array[String]): Unit =
    println(combinationSum(Array(2, 3, 6, 7), 7))
    println(combinationSum(Array(2, 3, 5), 8))
    println(combinationSum(Array(2), 1))
    println(combinationSum(Array(1), 1))
    println(combinationSum(Array(1), 2))

  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] =
    val mem    = mutable.Map.empty[(Int, Int), List[List[Int]]]
    val sorted = candidates.sorted

    def dfs(t: Int, minIdx: Int): List[List[Int]] =
      if minIdx == sorted.length || t < sorted(minIdx) then return List.empty

      if mem.contains((t, minIdx)) then return mem((t, minIdx))
      val result =
        if t == sorted(minIdx) then List(List(t))
        else
          val withCurrent =
            dfs(t - sorted(minIdx), minIdx).map(_.prepended(sorted(minIdx)))
          val withoutCurrent = dfs(t, minIdx + 1)
          withCurrent ++ withoutCurrent
      mem.update((t, minIdx), result)
      result

    dfs(target, 0)
