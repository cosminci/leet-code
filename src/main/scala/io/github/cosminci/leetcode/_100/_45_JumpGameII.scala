package io.github.cosminci.leetcode._100

object _45_JumpGameII:

  def main(args: Array[String]): Unit =
    println(jump(Array(0)))
    println(jump(Array(2, 3, 1, 1, 4)))

  def jump(nums: Array[Int]): Int =
    if nums.length == 1 then return 0

    var start    = 0
    var farthest = nums(0)

    var count = 1
    while farthest < nums.length - 1 do
      val prevStart = start
      start = farthest
      farthest = (prevStart to farthest).map(i => i + nums(i)).max
      count += 1

    count
