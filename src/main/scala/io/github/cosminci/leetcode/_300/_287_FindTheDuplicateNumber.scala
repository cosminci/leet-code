package io.github.cosminci.leetcode._300

object _287_FindTheDuplicateNumber:

  def main(args: Array[String]): Unit =
    println(findDuplicate(Array(1, 3, 4, 2, 2)))
    println(findDuplicate(Array(3, 1, 3, 4, 2)))
    println(findDuplicate(Array(1, 1)))
    println(findDuplicate(Array(1, 1, 2)))
    println(findDuplicate(Array(2, 5, 9, 6, 9, 3, 8, 9, 7, 1)))

  def findDuplicate(nums: Array[Int]): Int =
    var slow = nums(0)
    var fast = nums(slow)

    while nums(slow) != nums(fast) do
      slow = nums(slow)
      fast = nums(nums(fast))

    var slow2 = 0
    while nums(slow) != nums(slow2) do
      slow = nums(slow)
      slow2 = nums(slow2)

    nums(slow)
