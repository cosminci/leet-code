package com.leetcode.cosminci._1900

object _1887_ReductionOperationToMakeTheArrayElementsEqual:
  def main(args: Array[String]): Unit =
    println(reductionOperations(Array(5, 1, 3)))
    println(reductionOperations(Array(1, 1, 1)))
    println(reductionOperations(Array(1, 1, 2, 2, 3)))

  def reductionOperations(nums: Array[Int]): Int =
    nums.sortInPlace()
    val min = nums.head
    nums.indices.tail
      .foldLeft((0, 0)) { case ((count, prevFrequency), i) =>
        if nums(i) != nums(i - 1) then (count + prevFrequency + 1, prevFrequency + 1)
        else (count + prevFrequency, prevFrequency)
      }
      ._1
