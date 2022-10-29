package com.leetcode.cosminci._1100

object _1094_CarPooling:

  def carPooling(trips: Array[Array[Int]], capacity: Int): Boolean =
    trips
      .flatMap { case Array(people, from, to) => Array((from, people), (to, -people)) }
      .sorted
      .foldLeft(0) { case (currCapacity, (location, people)) =>
        if currCapacity + people > capacity then return false
        else currCapacity + people
      } >= 0
