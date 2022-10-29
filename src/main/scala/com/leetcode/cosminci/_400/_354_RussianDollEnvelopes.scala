package com.leetcode.cosminci._400

import scala.collection.Searching.Found

object _354_RussianDollEnvelopes:

  def maxEnvelopes(envelopes: Array[Array[Int]]): Int =
    envelopes
      .map { case Array(w, h) => (w, h) }
      .sortBy { case (w, h) => (w, -h) }
      .foldLeft(Array.empty[Int]) { case (dp, (_, h)) =>
        val idx = dp.search(h).insertionPoint
        if idx == dp.length then dp.appended(h)
        else dp.updated(idx, h)
      }
      .length
