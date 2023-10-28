package com.leetcode.cosminci._2900

object _2844_MinOpsToMakeSpecialNum:

  def minimumOperations(num: String): Int =
    val (_, zeroFound) = num.indices.foldRight(false, false) { case (i, (fiveFound, zeroFound)) =>
      if zeroFound && (num(i) == '0' || num(i) == '5') then return num.length - 2 - i
      if fiveFound && (num(i) == '2' || num(i) == '7') then return num.length - 2 - i
      (fiveFound || num(i) == '5', zeroFound || num(i) == '0')
    }
    num.length - (if zeroFound then 1 else 0)
