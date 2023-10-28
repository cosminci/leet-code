package com.leetcode.cosminci._2900

object _2843_CountSymmetricIntegers:

  def countSymmetricIntegers(low: Int, high: Int): Int =
    def isSymmetric(n: Int): Boolean =
      val s        = n.toString
      val (fh, sh) = s.splitAt(s.length / 2)
      s.length % 2 == 0 && fh.sum == sh.sum

    (low to high).count(isSymmetric)
