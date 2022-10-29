package com.leetcode.cosminci._2400

object _2379_MinRecolorToGetKConsecutiveBlocks:

  def minimumRecolors(blocks: String, k: Int): Int =
    blocks.sliding(k).map(_.count(_ == 'W')).min
