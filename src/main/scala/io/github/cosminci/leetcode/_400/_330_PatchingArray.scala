package io.github.cosminci.leetcode._400

object _330_PatchingArray {
  def main(args: Array[String]): Unit = {
    println(minPatches(Array(1, 2, 31, 33), Int.MaxValue))
    println(minPatches(Array(1, 3), 6))
    println(minPatches(Array(1, 5, 10), 20))
    println(minPatches(Array(1, 2, 2), 5))
  }

  private def minPatches(nums: Array[Int], n: Int): Int = {
    var (idx, patches, currentTarget) = (0, 0, 1L)

    while (currentTarget <= n) {
      if (idx < nums.length && nums(idx) <= currentTarget) {
        currentTarget += nums(idx)
        idx += 1
      } else {
        patches += 1
        currentTarget *= 2
      }
    }

    patches
  }
}
