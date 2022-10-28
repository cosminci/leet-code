package io.github.cosminci.leetcode._400

import scala.collection.mutable
import scala.util.Random

object _380_InsertDeleteGetRandom:

  class RandomizedSet:
    private val indices = mutable.Map.empty[Int, Int]
    private val values  = mutable.ArrayBuffer.empty[Int]

    def insert(value: Int): Boolean =
      indices.get(value) match
        case Some(_) =>
          false
        case None =>
          values.append(value)
          indices.update(value, values.length - 1)
          true

    def remove(value: Int): Boolean =
      indices.remove(value) match
        case None =>
          false
        case Some(idx) =>
          val last = values.remove(values.length - 1)
          if idx != values.length then
            values.update(idx, last)
            indices.update(last, idx)
          true

    def getRandom(): Int = values(Random.nextInt(values.length))
