package io.github.cosminci.leetcode._300

import io.github.cosminci.utils

import scala.collection.mutable

object _212_WordSearchII:
  def main(args: Array[String]): Unit =
    println(
      findWordsRecursiveDFS(
        Array(
          "oaan".toCharArray,
          "etae".toCharArray,
          "ihkr".toCharArray,
          "iflv".toCharArray
        ),
        Array("oath", "pea", "eat", "rain", "oaths", "rai", "raix")
      )
    )

  def findWordsRecursiveDFS(board: Array[Array[Char]], words: Array[String]): List[String] =
    val root    = buildPrefixTrie(words.sortBy(_.length))
    val matches = mutable.Set.empty[String]

    val visited = mutable.Set.empty[(Int, Int)]
    def dfs(x: Int, y: Int, node: TrieNode, prefix: String): Unit =
      if visited.contains((x, y)) || !node.next.contains(board(x)(y)) then return

      if node.next(board(x)(y)).terminator then matches.add(prefix + board(x)(y))

      visited.add((x, y))
      utils.neighbours(x, y, board).foreach { case (nx, ny) =>
        dfs(nx, ny, node.next(board(x)(y)), prefix + board(x)(y))
      }
      visited.remove((x, y))

    board.indices.foreach { x =>
      board(x).indices.foreach { y =>
        dfs(x, y, root, "")
      }
    }
    matches.toList

  private def buildPrefixTrie(words: Array[String]) =
    val root = TrieNode(mutable.Map.empty, terminator = false)
    words.foreach { w =>
      var node = root
      w.zipWithIndex.foreach { case (char, idx) =>
        if node.next.contains(char) then node = node.next(char)
        else {
          val newNode = TrieNode(mutable.Map.empty, terminator = idx == w.length - 1)
          node.next.update(char, newNode)
          node = newNode
        }
      }
    }
    root

  case class TrieNode(next: mutable.Map[Char, TrieNode], terminator: Boolean)
