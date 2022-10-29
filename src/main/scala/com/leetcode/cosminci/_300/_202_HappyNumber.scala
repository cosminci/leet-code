package com.leetcode.cosminci._300

import scala.collection.mutable

object _202_HappyNumber:
  def main(args: Array[String]): Unit =
    println(isHappySet(19))
    println(isHappyFloyd(19))

  def isHappySet(num: Int): Boolean =
    val seen = mutable.Set.empty[Int]
    var n    = num
    while seen.add(n) do
      n = digitSquareSum(n)
      if n == 1 then return true
    false

  def isHappyFloyd(num: Int): Boolean =
    var slow = num
    var fast = digitSquareSum(num)
    while slow != fast do
      slow = digitSquareSum(slow)
      fast = digitSquareSum(digitSquareSum(fast))
    slow == 1

  def digitSquareSum(num: Int): Int =
    var (sum, n) = (0, num)
    while n != 0 do
      sum += (n % 10) * (n % 10)
      n = n / 10
    sum
