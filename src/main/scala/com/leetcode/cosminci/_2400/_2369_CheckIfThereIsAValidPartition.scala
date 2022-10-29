package com.leetcode.cosminci._2400

import scala.collection.mutable

object _2369_CheckIfThereIsAValidPartition:

  def validPartition(nums: Array[Int]): Boolean =
    sealed trait Situation
    case object JustSplit          extends Situation
    case object Buffered           extends Situation
    case object Need3rdEqual       extends Situation
    case object Need3rdConsecutive extends Situation

    val mem = mutable.Map.empty[(Int, Situation), Boolean]
    def dfs(i: Int, situation: Situation): Boolean = mem.getOrElseUpdate((i, situation),
      if i == nums.length then situation == JustSplit
      else situation match
        case JustSplit => dfs(i + 1, Buffered)
        case Need3rdConsecutive =>
          nums(i) == nums(i - 1) + 1 && dfs(i + 1, JustSplit)
        case Need3rdEqual =>
          nums(i) == nums(i - 1) && dfs(i + 1, JustSplit)
        case Buffered =>
          if nums(i) == nums(i - 1) then dfs(i + 1, JustSplit) || dfs(i + 1, Need3rdEqual)
          else if nums(i) == nums(i - 1) + 1 then dfs(i + 1, Need3rdConsecutive)
          else false
    )

    dfs(i = 0, JustSplit)
