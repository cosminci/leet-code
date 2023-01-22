package com.leetcode.cosminci._2600

object _2543_CheckIfPointReachable:

  @annotation.tailrec
  def isReachable(x: Int, y: Int): Boolean =
    if x == 1 && y == 1 then true
    else if x % 2 == 0 then isReachable(x / 2, y)
    else if y % 2 == 0 then isReachable(x, y / 2)
    else if x > y then isReachable(x - y, y)
    else if y > x then isReachable(x, y - x)
    else false
