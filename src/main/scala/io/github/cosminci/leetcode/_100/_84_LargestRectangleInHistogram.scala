package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _84_LargestRectangleInHistogram:
  def main(args: Array[String]): Unit =
    println(largestRectangleArea(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)))

  private def largestRectangleArea(heights: Array[Int]): Int =
    var maxBlock          = Block(0, 0)
    val paddedHistogram   = heights.prepended(0).appended(0)
    val increasingHeights = mutable.Stack.empty[Block]
    increasingHeights.push(Block(0, 0))
    (1 until paddedHistogram.length).foreach { idx =>
      val currentHeight = paddedHistogram(idx)
      val prevHeight    = paddedHistogram(idx - 1)
      if currentHeight == prevHeight then
        increasingHeights.push(Block(currentHeight, increasingHeights.pop().width + 1))
      else if currentHeight > prevHeight then increasingHeights.push(Block(currentHeight, 1))
      else {
        var poppedLength = 0
        while increasingHeights.head.height > currentHeight do
          val prevBlock = increasingHeights.pop()
          if (prevBlock.width + poppedLength) * prevBlock.height > maxBlock.width * maxBlock.height then
            maxBlock = Block(prevBlock.height, prevBlock.width + poppedLength)
          poppedLength += prevBlock.width
        if currentHeight == increasingHeights.head.height then
          increasingHeights.push(Block(currentHeight, increasingHeights.pop().width + poppedLength + 1))
        else increasingHeights.push(Block(currentHeight, poppedLength + 1))
      }
    }
    maxBlock.width * maxBlock.height

  final case class Block(height: Int, width: Int)
