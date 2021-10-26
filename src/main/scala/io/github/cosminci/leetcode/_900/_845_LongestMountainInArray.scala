package io.github.cosminci.leetcode._900

object _845_LongestMountainInArray:
  def main(args: Array[String]): Unit =
    println(longestMountain(Array(2, 1, 4, 7, 3, 2, 5)))
    println(longestMountain(Array(2, 2, 2)))

  private def longestMountain(arr: Array[Int]): Int =
    (1 until arr.length).foldLeft(0, 0, 0) {
      case ((prevMax, prevAsc, prevDesc), i) =>
        val mountainEnded = prevDesc > 0 && arr(i - 1) < arr(i) || arr(i - 1) == arr(i)
        val (asc, desc)   = Option.when(mountainEnded)(0, 0).getOrElse(prevAsc, prevDesc)

        val newAsc  = asc + Option.when(arr(i - 1) < arr(i))(1).getOrElse(0)
        val newDesc = desc + Option.when(arr(i - 1) > arr(i))(1).getOrElse(0)
        val newMax  = Option.when(newAsc > 0 && newDesc > 0)(prevMax.max(newAsc + newDesc + 1)).getOrElse(prevMax)

        (newMax, newAsc, newDesc)
      }._1
