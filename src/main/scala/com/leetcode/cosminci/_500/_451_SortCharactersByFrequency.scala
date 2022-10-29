package com.leetcode.cosminci._500

import scala.collection.mutable

object _451_SortCharactersByFrequency:
  def frequencySort(s: String): String =
    s.groupBy(identity).values.toSeq.sortBy(-_.length).flatten.mkString
