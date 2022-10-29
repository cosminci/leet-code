package com.leetcode.cosminci._2300

import scala.util.{Failure, Success, Try}

object _2269_FindTheKBeautyOfNum:

  def divisorSubstrings(num: Int, k: Int): Int =
    num.toString.sliding(k).count(ss => ss.toInt > 0 && num % ss.toInt == 0)
