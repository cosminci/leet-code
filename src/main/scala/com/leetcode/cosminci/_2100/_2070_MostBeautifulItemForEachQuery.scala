package com.leetcode.cosminci._2100

object _2070_MostBeautifulItemForEachQuery:

  def maximumBeauty(items: Array[Array[Int]], queries: Array[Int]): Array[Int] =
    val ascPricesWithMaxPrefixBeauty = items
      .sortBy { case Array(price, beauty) => (price, beauty) }
      .scanLeft((0, 0)) { case ((prevPrice, maxBeauty), Array(price, beauty)) =>
        (price, beauty.max(maxBeauty))
      }

    queries.map { price =>
      val idx = ascPricesWithMaxPrefixBeauty.search((price, Int.MaxValue)).insertionPoint
      ascPricesWithMaxPrefixBeauty(idx - 1)._2
    }
