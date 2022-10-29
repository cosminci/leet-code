package com.leetcode.cosminci._2400

import scala.collection.mutable

object _2336_SmallestNumInInfiniteSet:

  class SmallestInfiniteSet:
    private val addedBack = mutable.TreeSet.empty[Int]
    private var current   = 1

    def popSmallest(): Int =
      addedBack.headOption match
        case None =>
          current += 1
          current - 1
        case Some(min) =>
          addedBack.remove(min)
          min

    def addBack(num: Int): Unit =
      if current > num then addedBack.add(num)
