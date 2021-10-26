package io.github.cosminci.leetcode._200

object _137_SingleNumberII:
  def main(args: Array[String]): Unit =
    println(singleNumber(Array(2, 2, 3, 2)))
    println(singleNumber(Array(0, 1, 0, 1, 0, 1, 99)))

  private def singleNumber(nums: Array[Int]): Int =
    (0 until 32).foldLeft(0) {
      case (answer, bitIdx) =>
        val bitCount = nums.foldLeft(0) {
          case (count, n) =>
            if ((n >> bitIdx) & 1) == 1 then (count + 1) % 3 else count
        }
        if bitCount != 0 then answer | (1 << bitIdx) else answer
    }
