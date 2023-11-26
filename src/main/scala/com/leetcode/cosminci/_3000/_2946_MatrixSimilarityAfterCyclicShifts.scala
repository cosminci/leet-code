package com.leetcode.cosminci._3000

object _2946_MatrixSimilarityAfterCyclicShifts:

  def areSimilar(mat: Array[Array[Int]], k: Int): Boolean =
    mat.forall { row =>
      row.indices.forall { c =>
        row(c) == row((c + k) % row.length)
      }
    }
