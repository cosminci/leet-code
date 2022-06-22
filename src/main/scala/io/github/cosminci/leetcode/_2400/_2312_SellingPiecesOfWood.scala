package io.github.cosminci.leetcode._2400

import scala.collection.mutable

object _2312_SellingPiecesOfWood:

  def sellingWood(height: Int, width: Int, prices: Array[Array[Int]]): Long =
    val priceMap = prices.map { case Array(h, w, p) => (h, w) -> p.toLong }.toMap
    val mem      = mutable.Map.empty[(Int, Int), Long]

    def dfs(height: Int, width: Int): Long = mem.getOrElseUpdate((height, width), {
      val verticalCuts   = (1 to width / 2).map(col => dfs(height, col) + dfs(height, width - col))
      val horizontalCuts = (1 to height / 2).map(row => dfs(row, width) + dfs(height - row, width))
      (horizontalCuts ++ verticalCuts ++ priceMap.get((height, width))).maxOption.getOrElse(0L)
    })

    dfs(height, width)
