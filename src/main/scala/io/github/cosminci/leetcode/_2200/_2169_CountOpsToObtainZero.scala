package io.github.cosminci.leetcode._2200

object _2169_CountOpsToObtainZero:
  def countOperations(num1: Int, num2: Int): Int =
    if num1 == 0 || num2 == 0 then 0
    else if num1 >= num2 then num1 / num2 + countOperations(num1 % num2, num2)
    else num2 / num1 + countOperations(num1, num2 % num1)
