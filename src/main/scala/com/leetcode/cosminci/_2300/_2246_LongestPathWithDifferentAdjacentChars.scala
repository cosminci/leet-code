package com.leetcode.cosminci._2300

object _2246_LongestPathWithDifferentAdjacentChars:

  def longestPath(parent: Array[Int], s: String): Int =
    val children = (1 until s.length).groupBy(parent).withDefaultValue(Seq.empty)

    def dfs(node: Int): (Int, Int) =
      val childData          = children(node).map(child => (dfs(child), child))
      val differentChildData = childData.collect { case (data, child) if s(child) != s(node) => data }

      val localMaxIfMiddle = differentChildData.sorted.takeRight(2).map(_._1).sum + 1
      val childGlobalMax   = childData.map { case ((_, globalChildMax), _) => globalChildMax }.maxOption.getOrElse(0)
      val newGlobalMax     = localMaxIfMiddle.max(childGlobalMax)
      val maxIfStart       = differentChildData.map(_._1).maxOption.getOrElse(0) + 1
      (maxIfStart, newGlobalMax)

    dfs(node = 0)._2
