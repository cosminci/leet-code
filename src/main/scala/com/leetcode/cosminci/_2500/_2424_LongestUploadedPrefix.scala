package com.leetcode.cosminci._2500

import scala.collection.mutable

object _2424_LongestUploadedPrefix:

  class LUPrefix(n: Int):
    private val pending   = mutable.Set.empty[Int]
    private var maxPrefix = 0

    def upload(video: Int) =
      pending.add(video)
      while pending.contains(maxPrefix + 1) do maxPrefix += 1

    def longest(): Int = maxPrefix
