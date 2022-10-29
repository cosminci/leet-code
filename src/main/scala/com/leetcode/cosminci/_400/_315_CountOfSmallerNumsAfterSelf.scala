package com.leetcode.cosminci._400

import com.leetcode.cosminci.utils

import scala.collection.mutable

object _315_CountOfSmallerNumsAfterSelf:

  def countSmaller(nums: Array[Int]): List[Int] =
    val sortedList = mutable.IndexedBuffer.empty[Int]
    val counts     = mutable.ListBuffer.empty[Int]
    nums.reverse.foreach { n =>
      val idx = utils.bisectLeft(nums, n)
      sortedList.insert(idx, n)
      counts.prepend(idx)
    }
    counts.toList
