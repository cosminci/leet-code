package io.github.cosminci.leetcode._2400

object _2358_MaxNumGroupsEnteringCompetition:

  def maximumGroups(grades: Array[Int]): Int =
    Iterator
      .iterate((0, 0)) { case (total, k) => (total + (k + 1), k + 1) }
      .dropWhile { case (total, k) => (total + k + 1) <= grades.length }
      .next()
      ._2
