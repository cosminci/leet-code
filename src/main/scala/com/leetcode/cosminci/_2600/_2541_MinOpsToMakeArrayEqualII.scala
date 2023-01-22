package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2541_MinOpsToMakeArrayEqualII:

  def minOperations(nums1: Array[Int], nums2: Array[Int], k: Int): Long =
    nums1.zip(nums2)
      .foldLeft(0L, 0) { case ((ops, balance), (n1, n2)) =>
        if n1 == n2 then (ops, balance)
        else if k == 0 || (n1 - n2) % k != 0 then return -1
        else (ops + ((n1 - n2) / k).abs, balance + ((n1 - n2) / k))
      }
      .pipe { case (ops, balance) => if balance == 0 then ops / 2 else -1 }
