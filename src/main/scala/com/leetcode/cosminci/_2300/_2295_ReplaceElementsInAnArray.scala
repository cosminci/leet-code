package com.leetcode.cosminci._2300

object _2295_ReplaceElementsInAnArray:

  def arrayChange(nums: Array[Int], operations: Array[Array[Int]]): Array[Int] =
    operations
      .foldLeft(nums.indices.zip(nums).toMap, nums.zipWithIndex.toMap) {
        case ((idxToNum, numToIdx), Array(prev, curr)) =>
          (idxToNum.updated(numToIdx(prev), curr), numToIdx.removed(prev).updated(curr, numToIdx(prev)))
      }
      ._1
      .toArray
      .sortBy { case (idx, _) => idx }
      .map { case (_, num) => num }
