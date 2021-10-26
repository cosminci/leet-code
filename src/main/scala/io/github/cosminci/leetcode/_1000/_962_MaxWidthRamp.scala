package io.github.cosminci.leetcode._1000

import java.util.TreeMap
import scala.collection.mutable

object _962_MaxWidthRamp:
  def main(args: Array[String]): Unit =
    println(maxWidthRampTreeMap(Array(9, 8, 1, 0, 1, 9, 4, 0, 4, 1)))
    println(maxWidthRampStack(Array(9, 8, 1, 0, 1, 9, 4, 0, 4, 5)))

  private def maxWidthRampTreeMap(nums: Array[Int]): Int =
    var max          = 0
    val prevElements = new TreeMap[Int, Int]()

    nums.indices.foreach { i =>
      if i == 0 || nums(i) < prevElements.firstKey() then prevElements.put(nums(i), i)
      else max = math.max(max, i - prevElements.floorEntry(nums(i)).getValue)
    }

    max

  private def maxWidthRampStack(nums: Array[Int]): Int =
    var max                    = 0
    val decreasingValueIndices = mutable.Stack.empty[Int]

    nums.indices.foreach { i =>
      if decreasingValueIndices.headOption.forall(h => nums(h) > nums(i)) then decreasingValueIndices.push(i)
    }
    nums.indices.reverse.foreach { i =>
      while decreasingValueIndices.headOption.exists(h => nums(h) <= nums(i)) do
        max = math.max(max, i - decreasingValueIndices.pop())
    }

    max
