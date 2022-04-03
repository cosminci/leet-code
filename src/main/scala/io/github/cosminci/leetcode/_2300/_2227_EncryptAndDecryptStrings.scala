package io.github.cosminci.leetcode._2300

object _2227_EncryptAndDecryptStrings:

  class Encrypter(keys: Array[Char], values: Array[String], dictionary: Array[String]):
    private val keyIndices = keys.zipWithIndex.toMap
    private val encCounts = dictionary
      .map(encrypt)
      .foldLeft(Map.empty[String, Int].withDefaultValue(0))((counts, s) => counts.updated(s, counts(s) + 1))

    def encrypt(word: String): String = word.map(c => values(keyIndices(c))).mkString

    def decrypt(word: String): Int = encCounts(word)
