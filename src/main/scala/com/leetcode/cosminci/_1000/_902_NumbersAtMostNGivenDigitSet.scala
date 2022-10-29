package com.leetcode.cosminci._1000

import scala.collection.mutable

object _902_NumbersAtMostNGivenDigitSet:
  def main(args: Array[String]): Unit =
    println(atMostNGivenDigitSet(Array("1", "4", "9"), 1000000000))

  def atMostNGivenDigitSet(rawDigits: Array[String], n: Long): Int =
    val digits = rawDigits.map(_.toInt)
    val num    = n.toString.map(_ - '0')

    val mem = mutable.Map.empty[(Int, Boolean, Boolean), Int]
    def dfs(i: Int, isPrefix: Boolean, isBigger: Boolean): Int =
      mem.getOrElseUpdate(
        (i, isPrefix, isBigger), {
          if i == num.length then Option.when(isBigger)(0).getOrElse(1)
          else if !isPrefix && !isBigger then 1 + digits.length * dfs(i + 1, isPrefix, isBigger)
          else 1 + digits.map(d => dfs(i + 1, isPrefix && d == num(i), isBigger || (isPrefix && d > num(i)))).sum
        }
      )

    dfs(i = 0, isPrefix = true, isBigger = false) - 1
