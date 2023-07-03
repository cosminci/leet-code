package com.leetcode.cosminci._900

object _859_BuddyStrings:

  def buddyStrings(s: String, goal: String): Boolean =
    if s.length != goal.length then false
    else if s == goal && s.distinct.length < s.length then true
    else
      s.zip(goal).filter { case (a, b) => a != b } match
        case Seq(diff1, diff2) => diff1 == diff2.swap
        case _                 => false
