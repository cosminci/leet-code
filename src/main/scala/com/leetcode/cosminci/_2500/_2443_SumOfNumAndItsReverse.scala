package com.leetcode.cosminci._2500

object _2443_SumOfNumAndItsReverse:

  def sumOfNumberAndReverse(num: Int): Boolean =
    @annotation.tailrec
    def reverse(n: Int, r: Int): Int =
      if n == 0 then r else reverse(n / 10, r * 10 + n % 10)

    (num / 2 to num).exists(n => n + reverse(n, r = 0) == num)
