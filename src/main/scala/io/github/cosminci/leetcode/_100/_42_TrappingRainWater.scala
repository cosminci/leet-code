package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _42_TrappingRainWater:

  def main(args: Array[String]): Unit =
    println(trap(Array(4, 2, 0, 3, 2, 5)))
    println(trapStack(Array(4, 2, 0, 3, 2, 5)))
    println(trap(Array(4, 2, 1, 0, 0, 3, 4)))
    println(trapStack(Array(4, 2, 1, 0, 0, 3, 4)))
    println(trap(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)))
    println(trapStack(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)))

  private def trap(heights: Array[Int]): Int = {
    if (heights.length == 0) return 0

    var waterTrapped        = 0
    var (left, right)       = (0, heights.length - 1)
    var (maxLeft, maxRight) = (heights(left), heights(right))

    while (left < right) {
      if (maxLeft <= maxRight) {
        left += 1
        maxLeft = math.max(maxLeft, heights(left))
        waterTrapped += math.max(0, maxLeft - heights(left))
      } else {
        right -= 1
        maxRight = math.max(maxRight, heights(right))
        waterTrapped += math.max(0, maxRight - heights(right))
      }
    }
    waterTrapped
  }

  private def trapStack(heights: Array[Int]): Int =
    val tracker = mutable.Stack.empty[Block]

    var waterTrapped = 0
    heights.foreach { h =>
      if tracker.headOption.forall(_.height > h) then tracker.push(Block(width = 1, height = h))
      else {
        var poppedWidth = 0
        var prevHeight  = tracker.head.height
        while tracker.headOption.exists(_.height <= h) do
          val lower = tracker.pop()
          if lower.height > prevHeight then
            waterTrapped += (lower.height - prevHeight) * poppedWidth
            prevHeight = lower.height
          poppedWidth += lower.width
        tracker.headOption.foreach { b =>
          if b.height > h && h > prevHeight then waterTrapped += (h - prevHeight) * poppedWidth
        }
        tracker.push(Block(width = poppedWidth + 1, height = h))
      }
    }
    waterTrapped

  case class Block(width: Int, height: Int)
