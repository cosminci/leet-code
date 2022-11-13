package com.leetcode.cosminci._2500

import scala.util.chaining.*

object _2465_NumDistinctAverages:

  def distinctAverages(nums: Array[Int]): Int =
    nums.sorted
      .splitAt(nums.length / 2)
      .pipe { case (fh, sh) =>
        fh.zip(sh.reverse)
          .map { case (x, y) => (x + y).toDouble / 2 }
          .distinct
          .length
      }
