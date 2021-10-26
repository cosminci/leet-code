package io.github.cosminci.leetcode._500

object _467_UniqueSubstringsInWraparoundString {
  def main(args: Array[String]): Unit = {
    println(findSubstringInWraproundString("a"))
    println(findSubstringInWraproundString("cac"))
    println(findSubstringInWraproundString("zab"))
  }

  private def findSubstringInWraproundString(p: String): Int =
    p.indices.foldLeft(Seq.fill(26)(0), 0) {
      case ((maxLengths, currLength), i) =>
        val charIdx = p(i) - 'a'
        val prevCharIdx = p(math.floorMod(i - 1, p.length)) - 'a'
        val isConsecutive = math.floorMod(charIdx - prevCharIdx, 26) == 1
        val newCurrLength = if (isConsecutive) currLength + 1 else 1
        val newMaxLengths = maxLengths.updated(charIdx, math.max(maxLengths(charIdx), newCurrLength))
        (newMaxLengths, newCurrLength)
    }._1.sum
}
