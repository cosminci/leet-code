package com.leetcode.cosminci._300

object _274_HIndex {
  def main(args: Array[String]): Unit = {
    Seq(
      Array(3, 0, 6, 1, 5),
      Array(1, 3, 1)
    ).foreach { citations =>
      println(hIndexSortAndFold(citations))
      println(hIndexCountingSort(citations))
    }
  }

  def hIndexSortAndFold(citations: Array[Int]): Int =
    citations.sorted.foldRight(0) {
      case (numCitations, hIdx) =>
        if (numCitations < hIdx) return hIdx
        math.min(hIdx + 1, numCitations)
    }

  def hIndexCountingSort(citations: Array[Int]): Int = {
    val n = citations.length
    citations.foldLeft(Seq.fill(n + 1)(0)) {
      case (counts, numCitations) =>
        if (numCitations > n)
          counts.updated(n, counts(n) + 1)
        else
          counts.updated(numCitations, counts(numCitations) + 1)
    }.zipWithIndex.foldRight(0) {
      case ((count, hIdx), total) =>
        if (total + count >= hIdx) return hIdx
        total + count
    }
  }

}
