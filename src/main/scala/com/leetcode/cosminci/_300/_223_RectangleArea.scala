package com.leetcode.cosminci._300

object _223_RectangleArea:

  def computeArea(ax1: Int, ay1: Int, ax2: Int, ay2: Int, bx1: Int, by1: Int, bx2: Int, by2: Int): Int =
    val overlapLength = 0.max(ax2.min(bx2) - bx1.max(ax1))
    val overlapHeight = 0.max(ay2.min(by2) - by1.max(ay1))

    (ax2 - ax1) * (ay2 - ay1) + (bx2 - bx1) * (by2 - by1) - (overlapHeight * overlapLength)
