package com.leetcode.cosminci._2500

object _2454_NextGreaterElementIV:

  def secondGreaterElement(nums: Array[Int]): Array[Int] =
    val n          = nums.length
    val nextBigger = Array.tabulate(n + 1)(i => if i < n - 1 then 0 else n)

    def findNextBigger(start: Int, low: Int) =
      Iterator
        .iterate(start)(nextBigger)
        .dropWhile(j => j < n && nums(j) <= low)
        .next()

    nums.indices.reverse.foreach(i => nextBigger(i) = findNextBigger(i + 1, nums(i)))

    nums.indices.map { i =>
      val j = findNextBigger(nextBigger(i) + 1, nums(i))
      if j < n then nums(j) else -1
    }.toArray
