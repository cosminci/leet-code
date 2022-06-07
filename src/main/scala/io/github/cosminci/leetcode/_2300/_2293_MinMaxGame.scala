package io.github.cosminci.leetcode._2300

object _2293_MinMaxGame:

  @annotation.tailrec
  def minMaxGame(nums: Array[Int]): Int =
    if nums.length == 1 then nums.head
    else
      minMaxGame(Array.tabulate(nums.length / 2) { i =>
        if i % 2 == 0 then nums(2 * i).min(nums(2 * i + 1))
        else nums(2 * i).max(nums(2 * i + 1))
      })
