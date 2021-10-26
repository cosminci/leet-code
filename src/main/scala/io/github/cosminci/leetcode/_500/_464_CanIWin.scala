package io.github.cosminci.leetcode._500

object _464_CanIWin:
  def main(args: Array[String]): Unit =
    println(canIWin(10, 11))
    println(canIWin(10, 0))
    println(canIWin(10, 1))
    println(canIWin(4, 6))

  private def canIWin(maxChoosableInteger: Int, desiredTotal: Int): Boolean =
    val mem   = Array.ofDim[Int](1 << 20)
    val total = maxChoosableInteger * (maxChoosableInteger + 1) / 2
    if total < desiredTotal then return false
    if desiredTotal <= maxChoosableInteger then return true
    if total == desiredTotal then return maxChoosableInteger % 2 == 1

    def dfs(chosenBitset: Int, desired: Int): Boolean =
      if mem(chosenBitset) != 0 then return mem(chosenBitset) > 0
      if desired <= 0 then return false

      val result = (0 until maxChoosableInteger).exists { choice =>
        (chosenBitset & (1 << choice)) == 0 && !dfs((chosenBitset | (1 << choice)), desired - choice - 1)
      }
      mem.update(chosenBitset, if result then 1 else -1)
      result

    dfs(chosenBitset = 0, desired = desiredTotal)
