package com.leetcode.cosminci._2800

import scala.collection.mutable
import scala.util.chaining.*

object _2719_CountOfIntegers:

  def count(num1: String, num2: String, minSum: Int, maxSum: Int): Int =
    val mod = 1_000_000_007

    val mem = mutable.Map.empty[(Int, Int), Int]
    def split(v: Int, len: Int): Int = mem.getOrElseUpdate((v, len),
      if v == 0 then 1
      else if v > 9 * len || v < 0 || len == 0 then 0
      else (0 to 9)
        .map(d => split(v - d, len - 1))
        .foldLeft(0L) { case (x, y) => (x + y) % mod }.toInt
    )

    def count(num: String, limit: Int): Long =
      num.zipWithIndex
        .foldLeft(if num.map(_ - '0').sum == limit then 1L else 0L, limit) { case ((res, limit), (ch, i)) =>
          val digit = ch - '0'
          (0 until digit.min(limit + 1))
            .foldLeft(res)((res, d) => res + split(limit - d, num.length - i - 1))
            .pipe(res => (res, limit - digit))
        }.pipe { case (res, _) => res }

    (minSum to maxSum)
      .map(limit => count(num2, limit) - count((BigInt(num1) - 1).toString, limit))
      .foldLeft(0L) { case (x, y) => (x + y) % mod }.toInt
