package io.github.cosminci.leetcode._1000

object _922_SortArrayByParityII:
  def main(args: Array[String]): Unit =
    println(sortArrayByParityII(Array(1, 1, 1, 2, 2, 2)).toSeq)

  def sortArrayByParityII(nums: Array[Int]): Array[Int] =
    def swap(i: Int, j: Int) =
      val tmp = nums(i)
      nums(i) = nums(j)
      nums(j) = tmp

    @annotation.tailrec
    def dfs(evenIdx: Int, oddIdx: Int): Array[Int] =
      if evenIdx >= nums.length || oddIdx >= nums.length then nums
      else if (nums(evenIdx) & 1) == 0 then dfs(evenIdx + 2, oddIdx)
      else if (nums(oddIdx) & 1) == 1 then dfs(evenIdx, oddIdx + 2)
      else {
        swap(evenIdx, oddIdx)
        dfs(evenIdx, oddIdx)
      }

    dfs(evenIdx = 0, oddIdx = 1)
