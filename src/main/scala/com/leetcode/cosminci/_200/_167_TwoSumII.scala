package com.leetcode.cosminci._200

import scala.collection.Searching.Found

object _167_TwoSumII:

  def twoSumBinarySearch(numbers: Array[Int], target: Int): Array[Int] =
    def search(i: Int): Option[Int] =
      numbers.search(target - numbers(i), from = i + 1, to = numbers.length) match
        case Found(j) => Some(j)
        case _        => None

    numbers.indices
      .find(i => search(i).isDefined)
      .flatMap(i => search(i).map(j => Array(i + 1, j + 1)))
      .getOrElse(Array.empty)

  def twoSumTwoPointers(numbers: Array[Int], target: Int): Array[Int] =
    @annotation.tailrec
    def dfs(l: Int, r: Int): Array[Int] =
      if l >= r then Array.empty
      else if numbers(l) + numbers(r) == target then Array(l + 1, r + 1)
      else if numbers(l) + numbers(r) > target then dfs(l, r - 1)
      else dfs(l + 1, r)

    dfs(l = 0, r = numbers.length - 1)
