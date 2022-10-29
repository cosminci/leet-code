package com.leetcode.cosminci._2100

object _2064_MinMaxOfProductsToAnyStore:
  def minimizedMaximum(n: Int, quantities: Array[Int]): Int =
    @annotation.tailrec
    def binarySearch(left: Int, right: Int): Int =
      if left >= right then left
      else
        val mid = left + (right - left) / 2
        if quantities.map(q => (q + mid - 1) / mid).sum > n then 
          binarySearch(mid + 1, right)
        else 
          binarySearch(left, mid)
          
    binarySearch(left = 1, right = quantities.max)
