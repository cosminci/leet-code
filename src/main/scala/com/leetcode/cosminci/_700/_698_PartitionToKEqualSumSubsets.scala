package com.leetcode.cosminci._700

object _698_PartitionToKEqualSumSubsets:
  def main(args: Array[String]): Unit =
    println(canPartitionKSubsets(Array(4, 3, 2, 3, 5, 2, 1), 4))

  def canPartitionKSubsets(nums: Array[Int], k: Int): Boolean =
    val total = nums.sum

    if total % k != 0 then return false
    val target  = total / k
    val buckets = Array.ofDim[Int](k)

    def dfs(idx: Int): Boolean =
      if idx == nums.length then return buckets.toSet.size == 1

      buckets.indices.foreach { bucketIdx =>
        buckets(bucketIdx) += nums(idx)
        if buckets(bucketIdx) <= target && dfs(idx + 1) then return true
        buckets(bucketIdx) -= nums(idx)
        if buckets(bucketIdx) == 0 then return false
      }
      false

    dfs(0)
