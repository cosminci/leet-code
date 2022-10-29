package com.leetcode.cosminci._1000

import scala.collection.mutable

object _923_3SumWithMultiplicity:
  def main(args: Array[String]): Unit =
    println(threeSumMulti(Array(1, 1, 2, 2, 3, 3, 4, 4, 5, 5), 8))

  def threeSumMulti(arr: Array[Int], target: Int): Int =
    val counts = mutable.Map.empty[Int, Int]
    val mod    = 1_000_000_007
    var result = 0
    arr.indices.foreach { i =>
      result = (result + counts.getOrElse(target - arr(i), 0)) % mod
      (0 until i).foreach { j =>
        counts.update(arr(i) + arr(j), counts.getOrElse(arr(i) + arr(j), 0) + 1)
      }
    }
    result
