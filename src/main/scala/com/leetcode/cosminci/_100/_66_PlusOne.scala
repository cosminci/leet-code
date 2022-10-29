package com.leetcode.cosminci._100

object _66_PlusOne:

  def plusOne(digits: Array[Int]): Array[Int] =
    val (res, carry) = digits.foldRight(Array.empty[Int], 1) { case (d, (res, carry)) =>
      if d + carry == 10 then (0 +: res, 1)
      else ((d + carry) +: res, 0)
    }
    if carry == 1 then 1 +: res else res
