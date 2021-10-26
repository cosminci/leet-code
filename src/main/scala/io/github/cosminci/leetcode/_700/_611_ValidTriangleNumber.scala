package io.github.cosminci.leetcode._700

object _611_ValidTriangleNumber:

  def main(args: Array[String]): Unit =
    println(triangleNumber(Array(2, 2, 3, 4)))
    println(triangleNumber(Array(4, 2, 3, 4)))

  private def triangleNumber(input: Array[Int]): Int =
    if input.length < 3 then return 0
    val nums  = input.sorted
    var count = 0
    (0 until nums.length - 2).foreach { edge1Idx =>
      (edge1Idx + 1 until nums.length - 1).foreach { edge2Idx =>
        var edge3Idx = edge2Idx + 1
        while edge3Idx < nums.length && nums(edge3Idx) < nums(edge1Idx) + nums(edge2Idx) do
          edge3Idx += 1
          count += 1
      }
    }
    count
