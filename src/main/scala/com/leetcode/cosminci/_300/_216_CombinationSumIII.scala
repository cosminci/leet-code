package com.leetcode.cosminci._300

import scala.collection.mutable

object _216_CombinationSumIII:
  def main(args: Array[String]): Unit =
    println(combinationSum3(3, 7))
    println(combinationSum3(3, 9))
    println(combinationSum3(4, 1))
    println(combinationSum3(9, 45))

  def combinationSum3(k: Int, n: Int): List[List[Int]] =
    def dfs(chosen: Seq[Int], min: Int): Seq[Seq[Int]] =
      if chosen.size == k && chosen.sum == n then Seq(chosen)
      else for
        choice <- min to 9 if chosen.sum + choice <= n && chosen.size < k
        combi  <- dfs(chosen :+ choice, choice + 1)
      yield combi

    dfs(chosen = List.empty, min = 1).map(_.toList).toList
