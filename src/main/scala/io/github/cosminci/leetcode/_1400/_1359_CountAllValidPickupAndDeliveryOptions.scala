package io.github.cosminci.leetcode._1400

object _1359_CountAllValidPickupAndDeliveryOptions:

  def countOrders(n: Int): Int =
    val factorial = (1 to n).foldLeft(1L)(_ * _ % 1_000_000_007)
    (1 until 2 * n by 2).foldLeft(factorial)(_ * _ % 1_000_000_007).toInt
