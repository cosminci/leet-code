package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _632_SmallestRangeCoveringElementsFromKLists:
  def main(args: Array[String]): Unit =
    println(smallestRange(List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))).toList)
    println(smallestRange(List(List(4, 10, 15, 24, 26), List(0, 9, 12, 20), List(5, 18, 22, 30))).toList)
    println(smallestRange(List(List(10, 10), List(11, 11))).toList)
    println(smallestRange(List(List(10), List(11))).toList)
    println(smallestRange(List(List(1), List(2), List(3), List(4), List(5), List(6), List(7))).toList)

  def smallestRange(nums: List[List[Int]]): Array[Int] =
    val rangeMax = math.pow(10, 5).toInt + 1
    val currentSet =
      given Ordering[(Int, Int)] = (x, y) => nums(y._2)(y._1).compare(nums(x._2)(x._1))
      mutable.PriorityQueue.from(nums.indices.map(idx => (0, idx)))
    var currentMax = nums.map(_.head).max
    var minRange   = Array(-rangeMax - 1, rangeMax + 1)

    while true do
      val (currentMinIdx, currentMinListIdx) = currentSet.dequeue()
      val currentMinList                     = nums(currentMinListIdx)
      if currentMax - currentMinList(currentMinIdx) < minRange.last - minRange.head then
        minRange(0) = currentMinList(currentMinIdx)
        minRange(1) = currentMax
      if currentMinIdx == currentMinList.length - 1 then return minRange
      else
        val nextFromMinList = currentMinList(currentMinIdx + 1)
        currentMax = math.max(currentMax, nextFromMinList)
        currentSet.enqueue((currentMinIdx + 1, currentMinListIdx))

    minRange
