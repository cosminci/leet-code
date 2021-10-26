package io.github.cosminci.leetcode._300

object _268_MissingNumber:
  def main(args: Array[String]): Unit =
    println(missingNumberXOR(Array(9, 6, 4, 2, 3, 5, 7, 0, 1)))
    println(missingNumberGauss(Array(9, 6, 4, 2, 3, 5, 7, 0, 1)))
    println(missingNumberSum(Array(9, 6, 4, 2, 3, 5, 7, 0, 1)))

  private def missingNumberXOR(nums: Array[Int]): Int =
    nums.indices.foldLeft(nums.length) { case (mask, idx) =>
      mask ^ idx ^ nums(idx)
    }

  private def missingNumberGauss(nums: Array[Int]): Int =
    nums.length * (nums.length + 1) / 2 - nums.sum

  private def missingNumberSum(nums: Array[Int]): Int =
    nums.indices.foldLeft(nums.length) { case (sum, idx) =>
      sum + idx - nums(idx)
    }
