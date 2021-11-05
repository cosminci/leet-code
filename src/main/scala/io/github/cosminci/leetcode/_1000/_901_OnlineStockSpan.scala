package io.github.cosminci.leetcode._1000

import scala.collection.mutable

object _901_OnlineStockSpan {

  class StockSpanner {
    private case class PriceSpan(price: Int, span: Int)
    private val prices = mutable.Stack.empty[PriceSpan]

    def next(price: Int): Int = {
      val span = prices.popWhile(_.price <= price).map(_.span).sum + 1
      prices.push(PriceSpan(price, span))
      span
    }
  }
}
