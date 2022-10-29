package com.leetcode.cosminci._200

object _187_RepeatedDNASequences:
  def main(args: Array[String]): Unit =
    println(findRepeatedDnaSequencesSliding("AAAAAAAAAAAAA"))
    println(findRepeatedDnaSequencesFold("AAAAAAAAAAAAA"))
    println(findRepeatedDnaSequencesSliding("AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT"))
    println(findRepeatedDnaSequencesFold("AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT"))

  def findRepeatedDnaSequencesSliding(s: String): List[String] =
    s.sliding(10).toSeq.groupBy(identity).filter(_._2.length > 1).keys.toList

  def findRepeatedDnaSequencesFold(s: String): List[String] =
    s.foldLeft(Set.empty[String], Set.empty[String], Set("")) {
      case ((duplicatedLength10Seqs, length10Seqs, incompleteSeqs), char) =>
        incompleteSeqs.foldLeft(duplicatedLength10Seqs, length10Seqs, Set.empty[String]) {
          case ((duplicated, length10, incomplete), seq) =>
            val newSeq = s"$seq$char"
            if seq.length == 9 then
              if length10.contains(newSeq) then (duplicated + newSeq, length10, incomplete)
              else (duplicated, length10 + newSeq, incomplete)
            else (duplicated, length10, incomplete + newSeq + s"$char")
        }
    }._1
      .toList
