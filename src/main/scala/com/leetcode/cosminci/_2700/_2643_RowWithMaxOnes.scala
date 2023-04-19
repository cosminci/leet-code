package com.leetcode.cosminci._2700

import scala.util.chaining.*

object _2643_RowWithMaxOnes:

  def rowAndMaximumOnes(mat: Array[Array[Int]]): Array[Int] =
    mat.indices
      .map(i => (i, mat(i).count(_ == 1)))
      .maxBy { case (i, count) => (count, -i) }
      .pipe { case (i, count) => Array(i, count) }
