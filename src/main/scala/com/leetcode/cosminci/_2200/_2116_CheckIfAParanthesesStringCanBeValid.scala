package com.leetcode.cosminci._2200

object _2116_CheckIfAParanthesesStringCanBeValid:

  def canBeValid(s: String, locked: String): Boolean =
    def validate(s: String, locked: String, p: Char): Boolean =
      val (required, budget) = s.zip(locked).foldLeft(0, 0) {
        case ((required, budget), (char, lock)) =>
          val newRequired = required + (if (lock == '0') 0 else if (char == p) 1 else -1)
          val newBudget   = budget + (if (lock == '0') 1 else 0)
          if newRequired + newBudget < 0 then return false
          (newRequired, newBudget)
      }
      required <= budget

    s.length % 2 == 0 && validate(s, locked, '(') && validate(s.reverse, locked.reverse, ')')
