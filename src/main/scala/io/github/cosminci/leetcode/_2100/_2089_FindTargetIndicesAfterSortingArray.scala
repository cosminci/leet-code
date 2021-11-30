package io.github.cosminci.leetcode._2100

object _2089_FindTargetIndicesAfterSortingArray {
  def targetIndices(nums: Array[Int], target: Int): List[Int] =
    nums.sorted.zipWithIndex.collect { case (n, i) if n == target => i }.toList

  def targetIndices2(nums: Array[Int], target: Int): List[Int] =
    (nums.count(_ < target) until nums.length - nums.count(_ > target)).toList
}
