package io.github.cosminci.leetcode._2200

object _2156_FindSubstringWithGivenHashValue:
  def main(args: Array[String]): Unit =
    println(subStrHash(s = "leetcode", power = 7, mod = 20, k = 2, targetHash = 0))

  def subStrHash(s: String, power: Int, mod: Int, k: Int, targetHash: Int): String =
    val value = (i: Int) => s(i) - '`'
    val len   = s.length
    val powK  = (1 to k).foldLeft(1L)((powK, _) => powK * power % mod)

    val initHash  = (len - k until len).foldRight(0L)((i, hash) => (hash * power + value(i)) % mod)
    val initStart = if initHash == targetHash then len - k else len

    val (_, start) = (0 until len - k).foldRight(initHash, initStart) { case (i, (prevHash, start)) =>
      val expandedHash = (prevHash * power + value(i))      % mod
      val shrunkHash   = expandedHash - value(i + k) * powK % mod
      val newHash      = math.floorMod(shrunkHash, mod)

      val newStart = if newHash == targetHash then i else start
      (newHash, newStart)
    }

    s.substring(start, start + k)
