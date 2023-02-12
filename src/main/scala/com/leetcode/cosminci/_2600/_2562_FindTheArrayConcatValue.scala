package com.leetcode.cosminci._2600

object _2562_FindTheArrayConcatValue:

  def findTheArrayConcVal(nums: Array[Int]): Long =
    @annotation.tailrec
    def dfs(nums: Seq[Int], result: Long): Long =
      nums match
        case first +: mid :+ last => dfs(mid, result + s"$first$last".toLong)
        case _                    => result + nums.sum

    dfs(nums.toSeq, result = 0L)
