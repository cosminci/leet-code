package io.github.cosminci.leetcode._2200

import scala.collection.mutable

object _2102_SequentiallyOrdinalRankTracker:

  class SORTracker:
    private val locations  = mutable.IndexedBuffer.empty[(Int, String)]
    private var queryCount = -1

    def add(name: String, score: Int): Unit =
      locations.insert(locations.search((-score, name)).insertionPoint, (-score, name))

    def get(): String =
      queryCount += 1
      locations(queryCount)._2
