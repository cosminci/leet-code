package com.leetcode.cosminci._1100

object _1010_PairsOfSongsWithTotalDurationsDivisibleBy60:
  def main(args: Array[String]): Unit =
    println(numPairsDivisibleBy60(Array(30, 20, 150, 100, 40)))
    println(numPairsDivisibleBy60(Array(60, 60, 60)))
    println(numPairsDivisibleBy60(Array(60, 60)))

  def numPairsDivisibleBy60(time: Array[Int]): Int =
    time
      .foldLeft((0, Map.empty[Int, Int])) { case ((totalCount, prevCounts), duration) =>
        val newTotal = totalCount + prevCounts.getOrElse((60 - duration % 60) % 60, 0)
        val newCounts = prevCounts.updatedWith(duration % 60) {
          case None    => Some(1)
          case Some(c) => Some(c + 1)
        }
        (newTotal, newCounts)
      }
      ._1
