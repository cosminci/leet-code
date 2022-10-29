package com.leetcode.cosminci._1300

object _1291_SequentialDigits:
  def sequentialDigits(low: Int, high: Int): List[Int] =
    (low.toString.length to high.toString.length).flatMap { digitCount =>
      (1 to 10 - digitCount)
        .map { firstDigit =>
          (1 until digitCount).foldLeft(firstDigit)((num, idx) => num * 10 + firstDigit + idx)
        }
        .filter(num => num >= low && num <= high)
    }.toList
