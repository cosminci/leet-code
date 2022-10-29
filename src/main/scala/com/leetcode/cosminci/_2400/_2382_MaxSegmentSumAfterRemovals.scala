package com.leetcode.cosminci._2400

import scala.collection.mutable

object _2382_MaxSegmentSumAfterRemovals:

  def maximumSegmentSum(nums: Array[Int], removeQueries: Array[Int]): Array[Long] =
    val idxToSegmentSumAndLen = mutable.Map.empty[Int, (Long, Int)].withDefaultValue((0L, 0))

    removeQueries.tail.scanRight(0L) { (idxToAdd, prevMax) =>
      idxToSegmentSumAndLen.update(idxToAdd, (nums(idxToAdd).toLong, 1))

      val (rSum, rLen) = idxToSegmentSumAndLen(idxToAdd + 1)
      val (lSum, lLen) = idxToSegmentSumAndLen(idxToAdd - 1)

      val total = nums(idxToAdd) + rSum + lSum
      idxToSegmentSumAndLen.update(idxToAdd + rLen, (total, lLen + rLen + 1))
      idxToSegmentSumAndLen.update(idxToAdd - lLen, (total, lLen + rLen + 1))

      prevMax.max(total)
    }
