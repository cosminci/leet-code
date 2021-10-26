package io.github.cosminci.leetcode._100

object _59_SpiralMatrixII:
  def main(args: Array[String]): Unit =
    println(generateMatrix(3).map(_.toSeq).toSeq)

  private def generateMatrix(n: Int): Array[Array[Int]] =
    val spiral         = Array.ofDim[Int](n, n)
    var (dr, dc, r, c) = (0, 1, 0, 0)
    (1 to n * n).foreach { v =>
      spiral(r)(c) = v
      if spiral((r + dr + n) % n)((c + dc + n) % n) != 0 then
        val tmp = dc
        dc = -dr
        dr = tmp
      r += dr
      c += dc
    }
    spiral
