package com.leetcode.cosminci._1000

import scala.collection.mutable

object _981_TimeBasedKVStore:

  class TimeMap:
    private val store = mutable.Map.empty[String, mutable.TreeMap[Int, String]]

    def set(key: String, value: String, ts: Int): Unit =
      store
        .getOrElseUpdate(key, mutable.TreeMap.empty)
        .update(ts, value)

    def get(key: String, ts: Int): String =
      store
        .get(key)
        .flatMap { values =>
          values
            .get(ts)
            .orElse(values.maxBefore(ts).map { case (_, v) => v })
        }
        .getOrElse("")
