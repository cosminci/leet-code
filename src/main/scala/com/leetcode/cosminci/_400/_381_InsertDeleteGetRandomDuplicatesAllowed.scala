package com.leetcode.cosminci._400

import scala.collection.mutable
import scala.util.Random

object _381_InsertDeleteGetRandomDuplicatesAllowed:

  class RandomizedCollection:
    private val indices = mutable.Map.empty[Int, mutable.Set[Int]]
    private val nums    = mutable.IndexedBuffer.empty[Int]

    def insert(value: Int): Boolean =
      val ind = indices.getOrElseUpdate(value, mutable.Set.empty)
      ind.add(nums.length)
      nums.append(value)
      ind.size == 1

    def remove(value: Int): Boolean =
      indices.get(value).flatMap(_.headOption) match
        case None => false
        case Some(removeIdx) =>
          indices(value).remove(removeIdx)
          val last = nums.last
          nums(removeIdx) = last
          indices(last).add(removeIdx)
          indices(last).remove(nums.length - 1)
          nums.remove(nums.length - 1)
          true

    def getRandom(): Int =
      nums(Random.nextInt(nums.length))
