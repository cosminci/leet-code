package io.github.cosminci.leetcode._700

object _775_GlobalAndLocalInversions {
  def main(args: Array[String]): Unit = {
    println(isIdealPermutation(Array(2, 1, 0)))
    println(isIdealPermutation(Array(2, 0, 1)))
    println(isIdealPermutation(Array(1, 0, 2)))
    println(isIdealPermutation(Array(1, 2, 0)))
  }

  def isIdealPermutation(nums: Array[Int]): Boolean =
    nums.indices.forall(i => math.abs(nums(i) - i) <= 1)
}
