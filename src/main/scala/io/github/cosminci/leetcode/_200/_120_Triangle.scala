package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _120_Triangle:
  def main(args: Array[String]): Unit =
    println(minimumTotalTopDown(List(List(2), List(3, 4), List(6, 5, 7), List(4, 1, 8, 3))))
    println(minimumTotalBottomUp(List(List(2), List(3, 4), List(6, 5, 7), List(4, 1, 8, 3))))

  def minimumTotalTopDown(triangle: List[List[Int]]): Int =
    val mem = mutable.Map.empty[(Int, Int), Int]

    def dfs(row: Int, idx: Int): Int =
      if row == triangle.length then return 0
      if mem.contains((row, idx)) then return mem((row, idx))

      val result = triangle(row)(idx) + math.min(dfs(row + 1, idx), dfs(row + 1, idx + 1))
      mem.update((row, idx), result)

      result

    dfs(0, 0)

  def minimumTotalBottomUp(triangle: List[List[Int]]): Int =
    (triangle.length - 1 to 0 by -1)
      .foldLeft(Array.ofDim[Int](triangle.last.length + 1).toSeq) { case (nextRow, rowIdx) =>
        triangle(rowIdx).indices.map(i => triangle(rowIdx)(i) + math.min(nextRow(i), nextRow(i + 1)))
      }
      .head
