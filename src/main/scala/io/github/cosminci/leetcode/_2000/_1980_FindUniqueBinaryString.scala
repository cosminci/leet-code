package io.github.cosminci.leetcode._2000

object _1980_FindUniqueBinaryString:
  def main(args: Array[String]): Unit =
    println(findDifferentBinaryString(Array("01", "10")))
    println(findDifferentBinaryString(Array("110", "011", "001")))

  def findDifferentBinaryString(nums: Array[String]): String =
    nums.indices.foldLeft("") { case (result, i) =>
      result :+ (if nums(i)(i) == '0' then '1' else '0')
    }
