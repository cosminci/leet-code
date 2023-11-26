package com.leetcode.cosminci._3000

object _2948_MakeLexicoSmallestArrBySwappingElems:

  def lexicographicallySmallestArray(nums: Array[Int], limit: Int): Array[Int] =
    val groups = nums.zipWithIndex.sorted.foldLeft(Array.empty[Array[Int]]) { case (groups, (n, j)) =>
      groups match
        case groups if groups.isEmpty                      => Array(Array(j))
        case groups if n - nums(groups.last.last) <= limit => groups.dropRight(1) :+ (groups.last :+ j)
        case _                                             => groups :+ Array(j)
    }
    val indexMap = groups.flatMap(group => group.sorted.zip(group)).toMap
    Array.tabulate(nums.length)(i => nums(indexMap(i)))
