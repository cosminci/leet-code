package io.github.cosminci.leetcode._300

object _223_RectangleArea:
  def main(args: Array[String]): Unit =
    println(computeArea(0, 0, 0, 0, -1, -1, 1, 1))
    println(computeArea(ax1 = -3, ay1 = 0, ax2 = 3, ay2 = 4, bx1 = 0, by1 = -1, bx2 = 9, by2 = 2))
    println(computeArea(ax1 = -3, ay1 = 0, ax2 = 3, ay2 = 4, bx1 = 4, by1 = -1, bx2 = 9, by2 = 2))
    println(computeArea(ax1 = -2, ay1 = -2, ax2 = 2, ay2 = 2, bx1 = -2, by1 = -2, bx2 = 2, by2 = 2))

  private def computeArea(ax1: Int, ay1: Int, ax2: Int, ay2: Int, bx1: Int, by1: Int, bx2: Int, by2: Int): Int =
    val overlapLength = math.max(0, math.min(ax2, bx2) - math.max(bx1, ax1))
    val overlapHeight = math.max(0, math.min(ay2, by2) - math.max(by1, ay1))

    (ax2 - ax1) * (ay2 - ay1) + (bx2 - bx1) * (by2 - by1) - (overlapHeight * overlapLength)
