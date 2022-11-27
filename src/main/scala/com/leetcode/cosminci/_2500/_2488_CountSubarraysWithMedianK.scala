package com.leetcode.cosminci._2500

import scala.util.chaining.*

object _2488_CountSubarraysWithMedianK:

  def countSubarrays(nums: Array[Int], k: Int): Int =
    def balance(i: Int) = if nums(i) > k then 1 else if nums(i) < k then -1 else 0

    val freqToLeft = (nums.indexOf(k) to 0 by -1)
      .foldLeft(Map.empty[Int, Int].withDefaultValue(0), 0) { case ((freq, runningBalance), i) =>
        (runningBalance + balance(i)).pipe { runningBalance =>
          (freq.updated(runningBalance, freq(runningBalance) + 1), runningBalance)
        }
      }
      .pipe { case (freq, _) => freq }

    (nums.indexOf(k) until nums.length)
      .foldLeft(0, 0) { case ((res, runningBalance), i) =>
        (runningBalance + balance(i)).pipe { runningBalance =>
          (res + freqToLeft(-runningBalance) + freqToLeft(-runningBalance + 1), runningBalance)
        }
      }
      .pipe { case (res, _) => res }
