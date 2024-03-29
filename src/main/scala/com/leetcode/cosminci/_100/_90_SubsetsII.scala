package com.leetcode.cosminci._100

object _90_SubsetsII:
  def main(args: Array[String]): Unit =
    println(subsetsWithDupRecursive(Array(4, 4, 4, 1, 4)))
    println(subsetsWithDupIterative(Array(4, 4, 4, 1, 4)))

  def subsetsWithDupIterative(nums: Array[Int]): List[List[Int]] =
    val digitCounts = nums.groupBy(identity).view.mapValues(_.size).toMap

    digitCounts.foldLeft(List(List.empty[Int])) { case (subsets, (digit, count)) =>
      val digitCombos = (0 to count).map(c => List.fill(c)(digit))
      subsets.flatMap { set =>
        digitCombos.map(set ++ _)
      }
    }

  def subsetsWithDupRecursive(nums: Array[Int]): List[List[Int]] =
    def dfs(i: Int): Set[Map[Int, Int]] =
      if i == nums.length then Set(Map.empty)
      else dfs(i + 1).flatMap { withoutCurrent =>
        val withCurrent = withoutCurrent.updated(nums(i), withoutCurrent.getOrElse(nums(i), 0) + 1)
        Set(withoutCurrent, withCurrent)
      }

    dfs(0)
      .map(_.foldLeft(List.empty[Int]) { case (acc, (digit, count)) => acc.prependedAll(Seq.fill(count)(digit)) })
      .toList
