package com.leetcode.cosminci._2500

object _2491_DividePlayersIntoTeamsOfEqualSkill:

  def dividePlayers(skill: Array[Int]): Long =
    val teamSkill = 2 * skill.sum / skill.length
    val skillFreq = skill.groupMapReduce(identity)(_ => 1)(_ + _)
    skillFreq.foldLeft(0L) { case (result, (playerSkill, playerCount)) =>
      if !skillFreq.get(teamSkill - playerSkill).contains(playerCount) then return -1
      else result + playerCount * playerSkill * (teamSkill - playerSkill)
    } / 2
