package io.github.cosminci.leetcode._300

object _238_ProductOfArrayExceptSelf:
  def main(args: Array[String]): Unit =
    println(productExceptSelf(Array(1, 2, 3, 4)).toSeq)
    println(productExceptSelf(Array(-1, 1, 0, -3, 3)).toSeq)

  def productExceptSelf(nums: Array[Int]): Array[Int] =
    val prefixProduct = nums.dropRight(1).scanLeft(1)(_ * _)
    val suffixProduct = nums.drop(1).scanRight(1)(_ * _)
    prefixProduct.zip(suffixProduct).map(_ * _)
