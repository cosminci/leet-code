package com.leetcode.cosminci._2500

object _2418_SortThePeople:

  def sortPeople(names: Array[String], heights: Array[Int]): Array[String] =
    names.indices
      .sortBy(i => -heights(i))
      .map(names)
      .toArray
