package io.github.cosminci.leetcode._800

object _719_FindKthSmallestPairDistance:
  def main(args: Array[String]): Unit =
    println(smallestDistancePair(Array(9, 10, 7, 10, 6, 1, 5, 4, 9, 8), 18))

  def smallestDistancePair(nums: Array[Int], k: Int): Int =
    nums.sortInPlace()

    def enoughSmallerPairs(value: Int) =
      var (count, l) = (0, 0)
      (1 until nums.length).foreach { r =>
        while nums(r) - nums(l) > value do l += 1
        count += (r - l)
      }
      count >= k

    var (low, high) = (0, nums.last - nums.head)
    while low < high do
      val mid = low + (high - low) / 2
      if enoughSmallerPairs(mid) then high = mid
      else low = mid + 1
    low
