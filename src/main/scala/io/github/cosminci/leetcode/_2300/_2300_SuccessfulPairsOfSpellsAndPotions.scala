package io.github.cosminci.leetcode._2300

object _2300_SuccessfulPairsOfSpellsAndPotions:

  def successfulPairs(spells: Array[Int], potions: Array[Int], success: Long): Array[Int] =
    potions.sortInPlace()

    @annotation.tailrec
    def search(l: Int = 0, r: Int, spell: Int): Int =
      if l >= r then potions.length - l
      else
        val mid = l + (r - l) / 2
        if spell.toLong * potions(mid) >= success then search(l, mid, spell)
        else search(mid + 1, r, spell)

    spells.map(search(l = 0, r = potions.length, _))
