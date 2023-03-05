package com.leetcode.cosminci._2600

import scala.math.Integral.Implicits.*

object _2582_PassThePillow:

  def passThePillow(n: Int, time: Int): Int =
    val (q, r) = time /% (n - 1)
    if q % 2 == 0 then r + 1 else n - r
