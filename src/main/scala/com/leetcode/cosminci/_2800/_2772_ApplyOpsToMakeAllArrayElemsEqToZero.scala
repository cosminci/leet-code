package com.leetcode.cosminci._2800

import scala.collection.mutable
import scala.util.chaining.*

object _2772_ApplyOpsToMakeAllArrayElemsEqToZero:

  def checkArray(nums: Array[Int], k: Int): Boolean =
    val balances     = mutable.IndexedBuffer.empty[Int]
    val balancesPSum = mutable.IndexedBuffer(0)
    nums.indices.foreach { i =>
      val newBalance = nums(i) - balancesPSum(i) + balancesPSum((i - k + 1).max(0))
      balances.append(newBalance)
      balancesPSum.append(balancesPSum.last + newBalance)
    }
    balances.forall(_ >= 0) && balances.takeRight(k - 1).forall(_ == 0)
