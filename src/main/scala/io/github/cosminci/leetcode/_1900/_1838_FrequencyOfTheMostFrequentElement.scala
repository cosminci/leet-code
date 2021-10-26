package io.github.cosminci.leetcode._1900

object _1838_FrequencyOfTheMostFrequentElement:
  def main(args: Array[String]): Unit =
    println(maxFrequency(Array(1, 2, 4, 1, 2, 1, 2, 2), 7))
    println(maxFrequency(Array(1, 2, 4), 5))
    println(maxFrequency(Array(1, 4, 8, 13), 5))

  def maxFrequency(nums: Array[Int], k: Int): Int =
    nums.sortInPlace()

    var (l, r)       = (0, 0)
    var (max, total) = (0, 0L)
    while r < nums.length do
      total += nums(r)
      while nums(r).toLong * (r - l + 1) > total + k do
        total -= nums(l)
        l += 1
      max = math.max(max, r - l + 1)
      r += 1
    max
