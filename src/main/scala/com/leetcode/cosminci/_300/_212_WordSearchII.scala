package com.leetcode.cosminci._300

import com.leetcode.cosminci.utils

import scala.collection.mutable

object _212_WordSearchII:

  def findWordsRecursiveDFS(board: Array[Array[Char]], words: Array[String]): List[String] =
    val root    = buildPrefixTrie(words.sortBy(_.length))
    val matches = mutable.ListBuffer.empty[String]

    def dfs(x: Int, y: Int, node: TrieNode): Unit =
      val ch = board(x)(y)
      node.next.get(ch) match
        case None           => ()
        case _ if ch == '#' => ()
        case Some(next) =>
          if next.word != null then
            matches.append(next.word)
            next.word = null // de-duplicate
          board(x)(y) = '#'
          utils.neighbours(x, y, board).foreach { case (nx, ny) => dfs(nx, ny, next) }
          board(x)(y) = ch

    board.indices.foreach { x =>
      board(x).indices.foreach { y =>
        dfs(x, y, root)
      }
    }
    matches.toList

  private def buildPrefixTrie(words: Array[String]) =
    val root = TrieNode()
    words.foldLeft(root) { (root, w) =>
      w.foldLeft(root) { (node, ch) =>
        node.next.getOrElseUpdate(ch, TrieNode())
      }.word = w
      root
    }
    root

  case class TrieNode(next: mutable.Map[Char, TrieNode] = mutable.Map.empty, var word: String = null)
