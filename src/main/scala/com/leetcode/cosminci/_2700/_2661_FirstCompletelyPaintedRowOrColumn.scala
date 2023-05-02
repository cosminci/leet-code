package com.leetcode.cosminci._2700

object _2661_FirstCompletelyPaintedRowOrColumn:

  def firstCompleteIndex(arr: Array[Int], mat: Array[Array[Int]]): Int =
    val (m, n) = (mat.length, mat.head.length)
    val indexMap = (0 until m).foldLeft(Map.empty[Int, (Int, Int)]) { (indexMap, r) =>
      (0 until n).foldLeft(indexMap) { (indexMap, c) =>
        indexMap.updated(mat(r)(c), (r, c))
      }
    }
    arr.indices.foldLeft(Map.empty[Int, Int], Map.empty[Int, Int]) { case ((rowCount, colCount), i) =>
      val (r, c)      = indexMap(arr(i))
      val newRowCount = rowCount.updated(r, rowCount.getOrElse(r, 0) + 1)
      val newColCount = colCount.updated(c, colCount.getOrElse(c, 0) + 1)
      if newRowCount(r) == n || newColCount(c) == m then return i
      (newRowCount, newColCount)
    }
    -1
