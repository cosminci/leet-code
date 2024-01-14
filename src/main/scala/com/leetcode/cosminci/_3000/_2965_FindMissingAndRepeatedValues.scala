package com.leetcode.cosminci._3000

object _2965_FindMissingAndRepeatedValues:

  def findMissingAndRepeatedValues(grid: Array[Array[Int]]): Array[Int] =
    val repeated = grid.flatten.groupMapReduce(identity)(_ => 1)(_ + _).collect { case (v, cnt) if cnt == 2 => v }
    val missing  = (1 to grid.length * grid.length).toSet.diff(grid.flatten.toSet)
    Array(repeated.head, missing.head)
