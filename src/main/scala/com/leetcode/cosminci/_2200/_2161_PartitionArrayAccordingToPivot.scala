package com.leetcode.cosminci._2200

object _2161_PartitionArrayAccordingToPivot {

  def pivotArray(nums: Array[Int], pivot: Int): Array[Int] =
    nums.filter(_ < pivot) ++ Array.fill(nums.count(_ == pivot))(pivot) ++ nums.filter(_ > pivot)
}
