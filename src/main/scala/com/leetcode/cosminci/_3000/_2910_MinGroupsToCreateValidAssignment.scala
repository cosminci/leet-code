package com.leetcode.cosminci._3000

import scala.math.Integral.Implicits.*

object _2910_MinGroupsToCreateValidAssignment:

  def minGroupsForValidAssignment(nums: Array[Int]): Int =
    val groupSizes = nums.groupMapReduce(identity)(_ => 1)(_ + _).values

    def split(minGroupsCandidate: Int): Int =
      groupSizes.map { size =>
        val (numGroups, rem) = size /% minGroupsCandidate
        if rem > numGroups then 100_000
        else (size.toFloat / (minGroupsCandidate + 1)).ceil.toInt
      }.sum

    if groupSizes.size == 1 then 1
    else (1 to groupSizes.min).map(split).min
