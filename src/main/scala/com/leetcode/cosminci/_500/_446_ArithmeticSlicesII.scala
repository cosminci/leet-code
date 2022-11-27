package com.leetcode.cosminci._500

import scala.util.chaining.*

object _446_ArithmeticSlicesII:

  def numberOfArithmeticSlices(nums: Array[Int]): Int =
    nums.map(_.toLong).pipe { nums =>
      nums.indices
        .foldLeft(Map.empty[(Int, Long), Int], 0) { case ((mem, res), j) =>
          (0 until j).foldLeft(mem, res) { case ((mem, res), i) =>
            val delta  = nums(j) - nums(i)
            val iCount = mem.getOrElse((i, delta), 0)
            val jCount = mem.getOrElse((j, delta), 0)
            (mem.updated((j, delta), iCount + jCount + 1), res + iCount)
          }
        }
        .pipe { case (_, result) => result }
    }
