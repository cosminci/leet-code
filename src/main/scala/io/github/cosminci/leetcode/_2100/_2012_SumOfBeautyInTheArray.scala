package io.github.cosminci.leetcode._2100

object _2012_SumOfBeautyInTheArray:
  def main(args: Array[String]): Unit =
    println(sumOfBeauties(Array(1, 2, 3)))
    println(sumOfBeauties(Array(2, 4, 6, 4)))
    println(sumOfBeauties(Array(3, 2, 1)))

  private def sumOfBeauties(nums: Array[Int]): Int =
    val maxPrefix = nums.scanLeft(0)(math.max).tail
    val minSuffix = nums.scanRight(Int.MaxValue)(math.min).dropRight(1)

    (1 until nums.length - 1).foldLeft(0) { case (beauty, idx) =>
      val newBeauty =
        if maxPrefix(idx - 1) < nums(idx) && nums(idx) < minSuffix(idx + 1) then 2
        else if nums(idx - 1) < nums(idx) && nums(idx) < nums(idx + 1) then 1
        else 0
      beauty + newBeauty
    }
