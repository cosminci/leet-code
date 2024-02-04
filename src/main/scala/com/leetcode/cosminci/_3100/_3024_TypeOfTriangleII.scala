package com.leetcode.cosminci._3100

object _3024_TypeOfTriangleII:

  def triangleType(nums: Array[Int]): String =
    if nums.sorted.take(2).sum <= nums.max then "none"
    else if nums.distinct.length == 1 then "equilateral"
    else if nums.distinct.length == 2 then "isosceles"
    else "scalene"
