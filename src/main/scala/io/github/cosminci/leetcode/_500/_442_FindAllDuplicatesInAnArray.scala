package io.github.cosminci.leetcode._500

object _442_FindAllDuplicatesInAnArray:
  def main(args: Array[String]): Unit =
    println(findDuplicates(Array(4, 3, 2, 7, 8, 2, 3, 1)))
    println(findDuplicates(Array(1, 1, 2)))
    println(findDuplicates(Array(1)))

  private def findDuplicates(nums: Array[Int]): List[Int] =
    nums.foldLeft(List.empty) { case (duplicates, n) =>
      val idx = math.abs(n) - 1
      if nums(idx) < 0 then duplicates :+ idx + 1
      else
        nums(idx) = -nums(idx)
        duplicates
    }
