package com.leetcode.cosminci._1000

import scala.collection.mutable

object _954_ArrayOfDoubledPairs:
  def main(args: Array[String]): Unit =
    println(canReorderDoubled(Array(4, -4, -1, -6, 8, -2, -4, -2, 4, 8, -3, -3, -2, -6)))
    println(canReorderDoubled(Array(3, 1, 3, 6)))
    println(canReorderDoubled(Array(2, 1, 2, 6)))
    println(canReorderDoubled(Array(4, -2, 2, -4)))
    println(canReorderDoubled(Array(1, 2, 4, 16, 8, 4)))

  def canReorderDoubled(arr: Array[Int]): Boolean =
    val counts = mutable.TreeMap.empty[Int, Int]
    arr.foreach { n =>
      counts.updateWith(n) {
        case None    => Some(1)
        case Some(c) => Some(c + 1)
      }
    }

    while counts.size > 0 do
      val (nextLowest, count) = counts.head
      if nextLowest < 0 && nextLowest % 2 != 0 then return false
      val pair = if nextLowest < 0 then nextLowest / 2 else nextLowest * 2

      counts.get(pair) match
        case None => return false
        case Some(c) =>
          if c < count then return false
          if c == count then counts.remove(pair)
          else counts.update(pair, c - count)
          counts.remove(nextLowest)

    return true
