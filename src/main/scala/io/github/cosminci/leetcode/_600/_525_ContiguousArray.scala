package io.github.cosminci.leetcode._600

import scala.collection.mutable

object _525_ContiguousArray:
  def main(args: Array[String]): Unit =
    println(findMaxLength(Array(0, 1, 1, 0, 1, 1, 1, 0)))

  private def findMaxLength(nums: Array[Int]): Int =
    nums.indices
      .foldLeft((Map(0 -> -1), 0, 0)) { case ((prevCounts, balance, maxLength), idx) =>
        val newBalance = balance + (if nums(idx) == 1 then 1 else -1)
        val (newCounts, newMaxLength) = prevCounts.get(newBalance) match
          case None =>
            (prevCounts.updated(newBalance, idx), maxLength)
          case Some(prevIdx) =>
            (prevCounts, math.max(maxLength, idx - prevIdx))
        (newCounts, newBalance, newMaxLength)
      }
      ._3
