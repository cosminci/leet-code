package io.github.cosminci.leetcode._600

object _523_ContinuousSubarraySum:

  def checkSubarraySum(nums: Array[Int], k: Int): Boolean =
    @annotation.tailrec
    def dfs(i: Int, prevRunningMod: Int, prevMods: Map[Int, Int]): Boolean =
      if i == nums.length then false
      else
        val runningMod = (prevRunningMod + nums(i)) % k
        if prevMods.get(runningMod).exists(j => i - j > 1) then true
        else
          dfs(i + 1, runningMod, prevMods.updatedWith(runningMod) {
            case None => Some(i)
            case j    => j
          })

    dfs(i = 0, prevRunningMod = 0, prevMods = Map(0 -> -1))
