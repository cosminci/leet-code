package com.leetcode.cosminci._3000

object _2966_DivideArrayIntoArraysWithMaxDiff:

  def divideArray(nums: Array[Int], k: Int): Array[Array[Int]] =
    val res = nums.sorted.grouped(3).toArray
    if res.forall(g => g.last - g.head <= k) then res
    else Array.empty
