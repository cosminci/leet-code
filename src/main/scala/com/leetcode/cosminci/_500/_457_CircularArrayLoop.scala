package com.leetcode.cosminci._500

import scala.collection.mutable

object _457_CircularArrayLoop:
  def main(args: Array[String]): Unit =
    println(circularArrayLoop(Array(-8, -1, 1, 7, 2)))
    println(circularArrayLoop(Array(-1, -1, -1)))
    println(circularArrayLoop(Array(-1, -2, -3, -4, -5)))
    println(circularArrayLoop(Array(2, -1, 1, 2, 2)))
    println(circularArrayLoop(Array(-1, 2)))
    println(circularArrayLoop(Array(-2, 1, -1, -2, -2)))

  def circularArrayLoop(nums: Array[Int]): Boolean =
    val n = nums.length

    def isCycleStart(start: Int): Boolean =
      val mark = 3000 + start
      var curr = start
      val num  = nums(start)

      while nums(curr) < 2000 && num * nums(curr) > 0 && nums(curr) % n != 0 do
        val next = nums(curr)
        nums(curr) = mark
        curr = math.floorMod(curr + next, n)
      nums(curr) == mark

    nums.indices.exists(i => nums(i) != 0 && isCycleStart(i))
