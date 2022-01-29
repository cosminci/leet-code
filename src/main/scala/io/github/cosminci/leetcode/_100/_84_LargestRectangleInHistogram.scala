package io.github.cosminci.leetcode._100

object _84_LargestRectangleInHistogram:
  def main(args: Array[String]): Unit =
    println(largestRectangleArea(Array(4, 2, 0, 3, 2, 4, 3, 4)))

  def largestRectangleArea(heights: Array[Int]): Int =
    (heights :+ 0).zipWithIndex
      .foldLeft(Seq.empty[Int], 0) { case ((stack, max), (currHeight, i)) =>
        @annotation.tailrec
        def dfs(stack: Seq[Int], max: Int): (Seq[Int], Int) =
          if stack.isEmpty || heights(stack.last) < currHeight then (stack :+ i, max)
          else
            val st :+ j    = stack
            val prevHigher = heights(j)
            val width      = i - st.lastOption.map(1 + _).getOrElse(0)
            dfs(st, max.max(prevHigher * width))

        dfs(stack, max)
      }
      ._2
