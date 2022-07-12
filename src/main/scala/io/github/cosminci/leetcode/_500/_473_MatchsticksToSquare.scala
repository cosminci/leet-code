package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _473_MatchsticksToSquare:

  def makesquare(nums: Array[Int]): Boolean =
    val sideLen = nums.sum / 4
    val mem     = mutable.Map.empty[Int, Int]

    def dfs(mask: Int): Int = mem.getOrElseUpdate(mask,
      if mask == 0 then 0
      else
        nums.indices
          .collect { case i if (mask & 1 << i) > 0 =>
            (i, dfs(mask ^ (1 << i)))
          }
          .collectFirst { case (i, sideLeft) if sideLeft >= 0 && sideLeft + nums(i) <= sideLen =>
            (sideLeft + nums(i)) % sideLen
          }
          .getOrElse(-1)
    )

    nums.sum % 4 == 0 && nums.forall(_ <= sideLen) && dfs(mask = (1 << nums.length) - 1) == 0
