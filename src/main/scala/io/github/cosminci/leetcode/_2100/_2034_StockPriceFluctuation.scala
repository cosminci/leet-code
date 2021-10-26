package io.github.cosminci.leetcode._2100

import scala.collection.mutable

object _2034_StockPriceFluctuation:

  class StockPrice:
    private val timestampToPrice = mutable.TreeMap.empty[Int, Int]
    private val priceCounts      = mutable.TreeMap.empty[Int, Int]

    def update(timestamp: Int, price: Int) =
      timestampToPrice.get(timestamp).foreach { prevPrice =>
        priceCounts.updateWith(prevPrice) {
          case None | Some(1) => None
          case Some(c)        => Some(c - 1)
        }
      }
      timestampToPrice.update(timestamp, price)
      priceCounts.update(price, priceCounts.getOrElse(price, 0) + 1)

    def current(): Int = timestampToPrice.last._2

    def maximum(): Int = priceCounts.lastKey

    def minimum(): Int = priceCounts.firstKey
