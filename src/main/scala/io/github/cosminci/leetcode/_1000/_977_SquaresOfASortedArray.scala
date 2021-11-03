package io.github.cosminci.leetcode._1000

import scala.collection.mutable

object _977_SquaresOfASortedArray:
  def main(args: Array[String]): Unit =
    println(sortedSquares(Array(-4, -1, 0, 3, 10)).toList)

  def sortedSquares(nums: Array[Int]): Array[Int] =
    var (l, r) = (0, nums.length - 1)
    val result = mutable.ListBuffer.empty[Int]

    while l <= r do
      if math.abs(nums(l)) > math.abs(nums(r)) then
        result.prepend(nums(l) * nums(l))
        l += 1
      else
        result.prepend(nums(r) * nums(r))
        r -= 1

    result.toArray
