package io.github.cosminci.leetcode._2200

object _2121_IntervalsBetweenIdenticalElements:
  def main(args: Array[String]): Unit =
    println(getDistances(Array(2, 1, 3, 1, 2, 3, 3)).toSeq)

  // Fails a few extreme test cases even though output is identical as far as Leetcode can print it
  def getDistances(arr: Array[Int]): Array[Long] =
    val result = Array.fill(arr.length)(0L)
    arr.indices.groupBy(arr).foreach { case (k, idxs) =>
      val pSum = idxs.scanLeft(0L)(_ + _)
      val rSum = idxs.scanRight(0L)(_ + _)
      idxs.zipWithIndex.foreach { case (idx, i) =>
        result(idx) = idx * i - pSum(i) + rSum(i) - idx * (idxs.length - i)
      }
    }
    result
