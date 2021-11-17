package io.github.cosminci.leetcode._2100

object _2075_DecodeTheSlantedCiphertext:
  def main(args: Array[String]): Unit =
    println(decodeCiphertext("ch   ie   pr", 3))

  def decodeCiphertext(encodedText: String, rows: Int): String =
    val size = encodedText.length
    val cols = size / rows

    (0 until cols).map { i =>
      (i until size by cols + 1).map(encodedText).mkString
    }.mkString.stripTrailing()
