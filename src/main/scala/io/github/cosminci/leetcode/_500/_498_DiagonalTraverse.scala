package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _498_DiagonalTraverse:
  def main(args: Array[String]): Unit =
    println(
      findDiagonalOrder(
        Array(
          Array(1, 2, 3),
          Array(4, 5, 6)
        )
      ).toSeq
    )

  private def findDiagonalOrder(mat: Array[Array[Int]]): Array[Int] =
    val (m, n) = (mat.length, mat.head.length)
    val coords = for
      x <- 0 until m
      y <- 0 until n
    yield (x, y)

    val diagonals = coords.foldLeft(Map.empty[Int, Seq[Int]]) {
      case (diags, (x, y)) =>
        diags.updatedWith(x + y) {
          case None         => Some(Seq(mat(x)(y)))
          case Some(values) => Some(values :+ mat(x)(y))
        }
    }

    (0 until m + n - 1)
      .foldLeft(Seq.empty[Int]) {
        case (results, diag) =>
          if diag % 2 == 0 then results ++ diagonals(diag).reverse
          else results ++ diagonals(diag)
      }
      .toArray
