package com.leetcode.cosminci._2900

object _2855_MinRightShiftsToSortArray:

  def minimumRightShifts(nums: List[Int]): Int =
    nums.indices.tail
      .collectFirst {
        case i if nums(i) < nums(i - 1) =>
          val (fh, sh) = nums.splitAt(i)
          if sh ++ fh == nums.sorted then nums.length - i else -1
      }.getOrElse(0)
