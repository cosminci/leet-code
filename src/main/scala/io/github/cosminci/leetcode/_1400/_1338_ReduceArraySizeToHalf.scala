package io.github.cosminci.leetcode._1400

object _1338_ReduceArraySizeToHalf:

  def main(args: Array[String]): Unit =
    println(minSetSize(Array(3, 3, 3, 3, 5, 5, 5, 2, 2, 7)))

  def minSetSize(arr: Array[Int]): Int =
    var remaining = arr.length
    arr
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .values
      .toList
      .sortWith { case (x, y) => x > y }
      .takeWhile { count =>
        remaining -= count
        remaining > arr.length / 2
      }
      .length + 1
