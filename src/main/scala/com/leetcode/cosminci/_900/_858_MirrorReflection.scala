package com.leetcode.cosminci._900

import com.leetcode.cosminci.utils.gcd

object _858_MirrorReflection {

  def mirrorReflection(p: Int, q: Int): Int =
    q / gcd(p, q) % 2 - p / gcd(p, q) % 2 + 1
}
