package io.github.cosminci.leetcode._1100

import scala.collection.mutable

object _1046_LastStoneWeight:
  def main(args: Array[String]): Unit =
    println(lastStoneWeight(Array(2, 7, 4, 1, 8, 1)))

  def lastStoneWeight(stones: Array[Int]): Int =
    val heaviestStones = mutable.PriorityQueue.from(stones)
    while heaviestStones.size > 1 do
      val (heaviest, secondHeaviest) = (heaviestStones.dequeue(), heaviestStones.dequeue())
      if heaviest > secondHeaviest then heaviestStones.enqueue(heaviest - secondHeaviest)
    heaviestStones.headOption.getOrElse(0)
