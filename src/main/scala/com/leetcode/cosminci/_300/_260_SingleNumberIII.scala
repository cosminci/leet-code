package com.leetcode.cosminci._300

object _260_SingleNumberIII:
  def main(args: Array[String]): Unit =
    println(singleNumber(Array(-1139700704, -1653765433)).toSeq)
    println(singleNumber(Array(1, 1, 2, 3, 2, 5)).toSeq)

  def singleNumber(nums: Array[Int]): Array[Int] =
    val targetXOR = nums.reduce(_ ^ _)
    val xorLSB    = targetXOR & -targetXOR
    nums.foldLeft(Array(0, 0)) { case (Array(target1, target2), n) =>
      if (n & xorLSB) == 0 then Array(target1 ^ n, target2) else Array(target1, target2 ^ n)
    }
