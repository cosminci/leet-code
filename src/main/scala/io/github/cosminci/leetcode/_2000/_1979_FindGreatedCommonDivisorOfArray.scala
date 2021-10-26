package io.github.cosminci.leetcode._2000

object _1979_FindGreatedCommonDivisorOfArray:
  def main(args: Array[String]): Unit =
    println(findGCD(Array(2, 5)))

  private def findGCD(nums: Array[Int]): Int =
    def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)
    gcd(nums.min, nums.max)
