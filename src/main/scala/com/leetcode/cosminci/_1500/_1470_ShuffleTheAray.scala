package com.leetcode.cosminci._1500

import scala.util.chaining._

object _1470_ShuffleTheAray:

  def shuffle(nums: Array[Int], n: Int): Array[Int] =
    nums.splitAt(n).pipe { case (fh, sh) =>
      fh.zip(sh).flatMap { case (a, b) => Array(a, b) }
    }
