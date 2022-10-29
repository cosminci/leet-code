package com.leetcode.cosminci._1100

import scala.collection.mutable

object _1074_NumberOfSubmatricesThatSumToTarget:

  def numSubmatrixSumTarget(matrix: Array[Array[Int]], target: Int): Int =
    val (m, n) = (matrix.length, matrix.head.length)
    val mtx    = matrix.map(r => r.tail.scanLeft(r.head)(_ + _))

    (0 until n).foldLeft(0) { (count, c0) =>
      (c0 until n).foldLeft(count) { (count, c1) =>
        (0 until m)
          .foldLeft(count, 0, Map(0 -> 1)) { case ((count, rollSum, pSums), r) =>
            val newRollSum = rollSum + mtx(r)(c1) - (if c0 > 0 then mtx(r)(c0 - 1) else 0)
            val newCount   = count + pSums.getOrElse(newRollSum - target, 0)
            val newPSums   = pSums.updated(newRollSum, pSums.getOrElse(newRollSum, 0) + 1)
            (newCount, newRollSum, newPSums)
          }
          ._1
      }
    }
