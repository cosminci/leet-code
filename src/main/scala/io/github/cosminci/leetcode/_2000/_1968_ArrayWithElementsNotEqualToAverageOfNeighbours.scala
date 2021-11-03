package io.github.cosminci.leetcode._2000

object _1968_ArrayWithElementsNotEqualToAverageOfNeighbours:
  def main(args: Array[String]): Unit =
    println(rearrangeArraySort(Array(1, 2, 3, 4, 5)).toList)
    println(rearrangeArraySwap(Array(1, 2, 3, 4, 5)).toList)

  def rearrangeArraySwap(nums: Array[Int]): Array[Int] =
    var order = nums(0) <= nums(1)
    (1 until nums.length - 1).foreach { i =>
      if order ^ (nums(i + 1) < nums(i)) then
        val tmp = nums(i)
        nums(i) = nums(i + 1)
        nums(i + 1) = tmp
      order = !order
    }
    nums

  def rearrangeArraySort(nums: Array[Int]): Array[Int] =
    nums.sorted.grouped(2).map(_.reverse).flatten.toArray
