package io.github.cosminci.leetcode._200

object _171_ExcelSheetColumnNumber {

  def titleToNumber(columnTitle: String): Int =
    columnTitle.foldLeft(0)((result, c) => 26 * result + c - 'A' + 1)
}
