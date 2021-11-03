package io.github.cosminci.leetcode._600

object _598_RangeAdditionII:
  def main(args: Array[String]): Unit =
    println(maxCount(3, 3, Array.empty))

  def maxCount(m: Int, n: Int, ops: Array[Array[Int]]): Int =
    val (minX, minY) = ops.foldLeft((m, n)) { case ((prevMinX, prevMinY), Array(x, y)) =>
      (math.min(prevMinX, x), math.min(prevMinY, y))
    }
    minX * minY
