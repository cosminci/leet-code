package com.leetcode.cosminci._2400

object _2319_CheckIfMatrixIsXMatrix:

  def checkXMatrix(grid: Array[Array[Int]]): Boolean =
    grid.indices.forall { x =>
      grid(x).indices.forall { y =>
        (x == y || (x + y) == grid.indices.last) ^ grid(x)(y) == 0
      }
    }
