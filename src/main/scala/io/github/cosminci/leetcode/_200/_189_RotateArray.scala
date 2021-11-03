package io.github.cosminci.leetcode._200

object _189_RotateArray:
  def main(args: Array[String]): Unit =
    val input = Array(1, 2)
    rotate(input, 3)
    println(input.toList)

  def rotate(nums: Array[Int], rotations: Int): Unit =
    val k = rotations % nums.length
    if k == 0 then return
    def reverse(start: Int, end: Int): Unit =
      (start to (end + start) / 2).indices.foreach { i =>
        val tmp = nums(start + i)
        nums(start + i) = nums(end - i)
        nums(end - i) = tmp
      }
    reverse(0, nums.length - 1)
    reverse(0, k - 1)
    reverse(k, nums.length - 1)
