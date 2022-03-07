package io.github.cosminci.leetcode._2200

object _2194_CellsInRangeOnExcelSheet:

  def cellsInRange(s: String): List[String] =
    (for
      col <- s(0) to s(3)
      row <- s(1) to s(4)
    yield s"$col$row").toList
