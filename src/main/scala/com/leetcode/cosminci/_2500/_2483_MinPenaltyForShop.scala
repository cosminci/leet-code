package com.leetcode.cosminci._2500

object _2483_MinPenaltyForShop:

  def bestClosingTime(customers: String): Int =
    val prefixPenalty = customers.scanLeft(0)((penalty, ch) => if ch == 'Y' then penalty else penalty + 1)
    val suffixPenalty = customers.scanRight(0)((ch, penalty) => if ch == 'N' then penalty else penalty + 1)
    (0 to customers.length).minBy(i => prefixPenalty(i) + suffixPenalty(i))
