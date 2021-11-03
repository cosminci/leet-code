package io.github.cosminci.leetcode._400

object _336_PalindromePairs:
  def main(args: Array[String]): Unit =
    println(palindromePairs(Array("a", "")))
    println(palindromePairs(Array("abcd", "dcba", "lls", "s", "sssll")))

  def palindromePairs(words: Array[String]): List[List[Int]] =
    val wordIndices = words.zipWithIndex.toMap

    val reversedPairs = wordIndices.collect {
      case (w, idx) if wordIndices.get(w.reverse).exists(_ != idx) =>
        List(idx, wordIndices(w.reverse))
    }.toList

    wordIndices.foldLeft(reversedPairs) { case (pairs, (w, idx)) =>
      val newPairs =
        if w == "" then
          wordIndices
            .filter { case (w, idx) => w.nonEmpty && w == w.reverse }
            .flatMap { case (_, idx2) =>
              List(List(idx2, idx), List(idx, idx2))
            }
        else
          (1 until w.length).toList.flatMap { i =>
            val (prefix, suffix)   = w.splitAt(i)
            val (prefixR, suffixR) = (prefix.reverse, suffix.reverse)
            val prefixPaliAndReversedSuffixMatch = Option.when(
              prefix == prefixR &&
                wordIndices.get(suffixR).exists(_ != idx)
            )(List(wordIndices(suffixR), idx))
            val suffixPaliAndReversedPrefixMatch = Option.when(
              suffix == suffixR &&
                wordIndices.get(prefixR).exists(_ != idx)
            )(List(idx, wordIndices(prefixR)))
            List.empty ++ prefixPaliAndReversedSuffixMatch ++ suffixPaliAndReversedPrefixMatch
          }
      pairs ++ newPairs
    }
