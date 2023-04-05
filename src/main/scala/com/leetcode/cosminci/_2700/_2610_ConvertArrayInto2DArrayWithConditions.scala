package com.leetcode.cosminci._2700

object _2610_ConvertArrayInto2DArrayWithConditions:

  def findMatrix(nums: Array[Int]): List[List[Int]] =
    val groups  = nums.groupBy(identity).values.map(_.toList).toList
    val maxSize = groups.map(_.length).max
    groups.map(_.padTo(maxSize, -1)).transpose.map(_.filterNot(_ == -1))
