package io.github.cosminci.leetcode._2400

import scala.collection.immutable.TreeSet
import scala.util.chaining.*

object _2398_MaxRobotsWithinBudget:

  def maximumRobots(chargeTimes: Array[Int], runningCosts: Array[Int], budget: Long): Int =
    runningCosts.indices
      .foldLeft(-1, 0L, TreeSet.empty[(Int, Int)], 0) { case ((left, sum, window, prevMax), right) =>
        Iterator
          .iterate((left, sum + runningCosts(right), window + (chargeTimes(right) -> right))) {
            case (left, sum, window) =>
              val next = left + 1
              (next, sum - runningCosts(next), window - (chargeTimes(next) -> next))
          }
          .dropWhile { case (_, sum, window) =>
            window.maxOption.map(_._1).getOrElse(0) + (right - left) * sum > budget
          }
          .next()
          .pipe { case (left, sum, window) => (left, sum, window, prevMax.max(right - left)) }
      }
      ._4
