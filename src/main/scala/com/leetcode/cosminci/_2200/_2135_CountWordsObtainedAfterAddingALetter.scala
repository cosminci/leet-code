package com.leetcode.cosminci._2200

object _2135_CountWordsObtainedAfterAddingALetter:

  def wordCount(startWords: Array[String], targetWords: Array[String]): Int =
    def bitset(word: String) = word.foldLeft(0)((bitset, char) => bitset | (1 << (char - 'a')))

    val startBitsets = startWords.map(bitset).toSet

    targetWords.count { word =>
      word.exists { char =>
        val bitsetWithoutChar = bitset(word) & ~(1 << (char - 'a'))
        startBitsets.contains(bitsetWithoutChar)
      }
    }
