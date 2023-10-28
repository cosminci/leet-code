package com.leetcode.cosminci._2900

object _2860_HappyStudents:

  def countWays(students: List[Int]): Int =
    val nums = students.sorted
    nums.indices.foldLeft(if nums.head == 0 then 0 else 1) { (ways, i) =>
      if i + 1 > nums(i) && (i + 1 == nums.length || i + 1 < nums(i + 1)) then ways + 1 else ways
    }
