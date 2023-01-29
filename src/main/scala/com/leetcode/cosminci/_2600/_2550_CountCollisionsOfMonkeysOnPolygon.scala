package com.leetcode.cosminci._2600

import com.leetcode.cosminci.utils

object _2550_CountCollisionsOfMonkeysOnPolygon:

  def monkeyMove(n: Int): Int =
    val mod    = 1_000_000_007
    val powMod = utils.powMod(base = 2, pow = n, mod = mod)
    math.floorMod(powMod - 2, mod)
