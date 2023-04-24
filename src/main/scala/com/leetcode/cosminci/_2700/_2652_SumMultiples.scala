package com.leetcode.cosminci._2700

object _2652_SumMultiples:

  def sumOfMultiples(n: Int): Int =
    (1 to n).filter(x => Seq(3, 5, 7).exists(x % _ == 0)).sum
