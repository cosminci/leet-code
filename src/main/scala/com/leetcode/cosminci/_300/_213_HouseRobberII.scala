package com.leetcode.cosminci._300

object _213_HouseRobberII:
  def main(args: Array[String]): Unit =
    println(rob(Array(2, 3, 2)))
    println(rob(Array(1, 2, 3, 1)))
    println(rob(Array(0)))

  def rob(nums: Array[Int]): Int =
    if nums.length == 1 then return nums.head

    def robHouses(houses: Array[Int]) =
      var (prev2, prev1) = (0, 0)
      houses.indices.foreach { i =>
        val newPrev1 = math.max(houses(i) + prev2, prev1)
        prev2 = prev1
        prev1 = newPrev1
      }
      prev1

    math.max(robHouses(nums.tail), robHouses(nums.dropRight(1)))
