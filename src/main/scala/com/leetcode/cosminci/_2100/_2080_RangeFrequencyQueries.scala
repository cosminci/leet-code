package com.leetcode.cosminci._2100

object _2080_RangeFrequencyQueries:
  def main(args: Array[String]): Unit =
    val rfq = new RangeFreqQuery(Array(33, 4, 4, 56, 22, 2, 34, 33, 22, 12, 34, 33))
    println(rfq.query(1, 2, 4))
    println(rfq.query(0, 11, 33))

  class RangeFreqQuery(nums: Array[Int]):
    val indices = nums.indices.groupBy(i => nums(i)).withDefaultValue(Seq.empty)

    def query(left: Int, right: Int, value: Int): Int =
      indices(value).search(right + 1).insertionPoint - indices(value).search(left).insertionPoint
