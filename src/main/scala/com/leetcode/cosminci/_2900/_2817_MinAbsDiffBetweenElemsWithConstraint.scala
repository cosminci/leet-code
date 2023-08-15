package com.leetcode.cosminci._2900

import scala.collection.immutable.TreeMap
import scala.util.chaining.*

object _2817_MinAbsDiffBetweenElemsWithConstraint:

  def minAbsoluteDifference(nums: List[Int], x: Int): Int =
    if x == 0 then 0
    else
      val window = TreeMap.from(nums.drop(x).groupMapReduce(identity)(_ => 1)(_ + _))
      nums.indices
        .dropRight(x)
        .foldLeft(Int.MaxValue, window) { case ((res, window), i) =>
          val minNext = window.minAfter(nums(i) - 1).map { case (closest, _) => (nums(i) - closest).abs }.getOrElse(res)
          val maxPrev = window.maxBefore(nums(i) + 1).map { case (closest, _) => (nums(i) - closest).abs }.getOrElse(res)
          (res.min(minNext).min(maxPrev), window.updatedWith(nums(i + x)) {
              case Some(c) if c > 1 => Some(c - 1)
              case _                => None
          })
        }.pipe { case (res, _) => res }
