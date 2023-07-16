package com.leetcode.cosminci._1200

import scala.collection.mutable

object _1125_SmallestSufficientTeam:

  def smallestSufficientTeam(reqSkills: Array[String], people: List[List[String]]): Array[Int] =
    val neededSkills = reqSkills.zipWithIndex.toMap
    val peopleSkills = people.map(_.foldLeft(0)((bitmask, skill) => bitmask | (1 << neededSkills(skill))))

    val mem = mutable.Map.empty[(Int, Int), Array[Int]]
    def dfs(i: Int, missingSkillsMask: Int): Array[Int] = mem.getOrElseUpdate((i, missingSkillsMask),
      if missingSkillsMask == 0 then Array.empty[Int]
      else if i == people.length then Array.fill(100)(0)
      else if (missingSkillsMask & peopleSkills(i)) == 0 then dfs(i + 1, missingSkillsMask)
      else Seq(dfs(i + 1, missingSkillsMask), dfs(i + 1, missingSkillsMask & ~peopleSkills(i)) :+ i).minBy(_.length)
    )

    dfs(i = 0, missingSkillsMask = (1 << reqSkills.length) - 1)
