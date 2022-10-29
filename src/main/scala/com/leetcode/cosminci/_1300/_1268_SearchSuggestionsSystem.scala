package com.leetcode.cosminci._1300

import scala.collection.mutable

object _1268_SearchSuggestionsSystem:

  case class TrieNode(
      next: mutable.Map[Char, TrieNode] = mutable.Map.empty,
      words: mutable.ListBuffer[String] = mutable.ListBuffer.empty
  )

  def suggestedProducts(products: Array[String], searchWord: String): List[List[String]] =
    val root = products.sorted.foldLeft(TrieNode()) { (root, product) =>
      product.foldLeft(root) { (node, char) =>
        val next = node.next.getOrElseUpdate(char, TrieNode())
        if next.words.length < 3 then next.words.append(product)
        next
      }
      root
    }

    searchWord.foldLeft(List.empty[List[String]], root) {
      case ((suggestions, node), char) =>
        node.next.get(char) match
          case Some(next) => (suggestions :+ next.words.toList, next)
          case None       => return suggestions.padTo(searchWord.length, List.empty)
      }
      ._1
