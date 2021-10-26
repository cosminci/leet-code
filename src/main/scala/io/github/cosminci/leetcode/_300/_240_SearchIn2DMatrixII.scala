package io.github.cosminci.leetcode._300

object _240_SearchIn2DMatrixII {
  def main(args: Array[String]): Unit = {
    println(searchMatrix(Array(
      Array(1, 4, 7, 11, 15),
      Array(2, 5, 8, 12, 19),
      Array(3, 6, 9, 16, 22),
      Array(10, 13, 14, 17, 24),
      Array(18, 21, 23, 26, 30)
    ), 30))
  }

  private def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    var (topRow, lastCol) = (0, matrix.head.length - 1)
    while (topRow < matrix.length && lastCol >= 0) {
      if (matrix(topRow)(lastCol) == target) return true
      if (matrix(topRow)(lastCol) > target)
        lastCol -= 1
      else
        topRow += 1
    }
    return false
  }
}
