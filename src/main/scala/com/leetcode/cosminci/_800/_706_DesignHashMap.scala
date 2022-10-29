package com.leetcode.cosminci._800

import scala.collection.mutable

object _706_DesignHashMap:
  
  class MyHashMap():
    private val buckets        = Array.fill(1000)(mutable.IndexedBuffer.empty[(Int, Int)])
    private def hash(key: Int) = ((key * 1031237) & 1 << 20) % 5

    def put(key: Int, value: Int): Unit =
      val bucket = hash(key) % buckets.length
      if buckets(bucket).exists(_._1 == key) then remove(key)
      buckets(bucket).addOne((key, value))

    def get(key: Int): Int =
      val bucket = hash(key) % buckets.length
      buckets(bucket).find(_._1 == key).map(_._2).getOrElse(-1)

    def remove(key: Int): Unit =
      val bucket = hash(key) % buckets.length
      val idx    = buckets(bucket).indexWhere(_._1 == key)
      if idx != -1 then buckets(bucket).remove(idx)
