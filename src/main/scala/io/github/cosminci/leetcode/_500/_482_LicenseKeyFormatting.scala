package io.github.cosminci.leetcode._500

object _482_LicenseKeyFormatting:
  def main(args: Array[String]): Unit =
    println(licenseKeyFormatting("5F3Z-2e-9-w", 4))
    println(licenseKeyFormatting2("5F3Z-2e-9-w", 4))
    println(licenseKeyFormatting("2-5g-3-J", 3))
    println(licenseKeyFormatting2("2-5g-3-J", 3))

  def licenseKeyFormatting(s: String, k: Int): String =
    s.collect { case char if char != '-' => char.toUpper }
      .reverse
      .grouped(k)
      .map(_.reverse)
      .toList
      .reverse
      .mkString("-")

  def licenseKeyFormatting2(input: String, k: Int): String =
    var dashes  = 0
    val builder = new StringBuilder
    val s       = input.filter(_ != '-')
    (s.length - 1 to 0 by -1).foreach { idx =>
      builder.append(s(idx).toUpper)
      if (builder.length() - dashes) % k == 0 && idx != 0 then
        builder.append('-')
        dashes += 1
    }
    builder.reverse.toString
