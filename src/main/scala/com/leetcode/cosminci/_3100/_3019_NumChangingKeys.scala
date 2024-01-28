package com.leetcode.cosminci._3100

object _3019_NumChangingKeys:

  def countKeyChanges(s: String): Int =
    s.indices.tail.count(i => s(i).toLower != s(i - 1).toLower)
