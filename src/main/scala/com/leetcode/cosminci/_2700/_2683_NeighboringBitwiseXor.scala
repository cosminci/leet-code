package com.leetcode.cosminci._2700

object _2683_NeighboringBitwiseXor:

  def doesValidArrayExist(derived: Array[Int]): Boolean =
    Seq(0, 1).exists { first =>
      derived.dropRight(1).foldLeft(first)((prev, xor) => prev ^ xor) == (first ^ derived.last)
    }
