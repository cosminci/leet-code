package com.leetcode.cosminci._500

import scala.collection.mutable

object _460_LFUCache:

  class LFUCache(capacity: Int):
    private val cache      = mutable.HashMap[Int, (Int, Int)]()
    private val lruTracker = mutable.HashMap[Int, mutable.LinkedHashSet[Int]]()
    private var minFreq    = -1

    def get(key: Int): Int =
      cache.get(key) match
        case None => -1
        case Some((value, freq)) =>
          cache.update(key, (value, freq + 1))
          lruTracker(freq).remove(key)
          if freq == minFreq && lruTracker(freq).isEmpty then minFreq += 1
          lruTracker.getOrElseUpdate(freq + 1, mutable.LinkedHashSet()).add(key)
          value

    def put(key: Int, value: Int): Unit =
      if capacity == 0 then ()
      else
        cache.get(key) match
          case Some((_, freq)) =>
            cache.put(key, (value, freq))
            get(key)
          case None =>
            if cache.size == capacity then
              val evict = lruTracker(minFreq).head
              lruTracker(minFreq).remove(evict)
              cache.remove(evict)
            cache.update(key, (value, 1))
            minFreq = 1
            lruTracker.getOrElseUpdate(1, mutable.LinkedHashSet()).add(key)
