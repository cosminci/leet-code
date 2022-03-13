package io.github.cosminci.leetcode._2200

object _2200_FindAllKDistantIndicesInArray:
  def findKDistantIndices(nums: Array[Int], key: Int, k: Int): List[Int] =
    val keys = -10000 +: nums.indices.filter(nums(_) == key) :+ 10000
    nums.indices
      .foldLeft(Seq.empty[Int], 0) { case ((result, left), i) =>
        val newResult = if i - keys(left) <= k || keys(left + 1) - i <= k then result :+ i else result
        val newLeft   = if keys(left + 1) == i then left + 1 else left
        (newResult, newLeft)
      }._1.toList
