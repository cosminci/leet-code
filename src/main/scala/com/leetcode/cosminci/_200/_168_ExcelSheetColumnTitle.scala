package com.leetcode.cosminci._200

object _168_ExcelSheetColumnTitle:

  def convertToTitle(columnNumber: Int): String =
    def dfs(n: Int): String =
      if n == 0 then "" else s"${dfs((n - 1) / 26)}${('A' + (n - 1) % 26).toChar}"

    dfs(columnNumber)
