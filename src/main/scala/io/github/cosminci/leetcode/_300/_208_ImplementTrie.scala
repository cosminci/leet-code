package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _208_ImplementTrie:
  def main(args: Array[String]): Unit =
    val trie = new Trie()
    trie.insert("apple")
    println(trie.search("apple"))
    println(trie.search("app"))
    println(trie.startsWith("app"))
    trie.insert("app")
    println(trie.search("app"))

  class Trie():
    private val root = Node(children = mutable.Map.empty)

    def insert(word: String): Unit =
      word.indices
        .foldLeft(root) { case (node, i) =>
          node.children.get(word(i)) match
            case Some(n) => n
            case None =>
              val next = Node(children = mutable.Map.empty)
              node.children.update(word(i), next)
              next
        }
        .terminator = true

    def search(word: String): Boolean =
      word.toCharArray
        .foldLeft(root) { case (node, char) =>
          if node.children.contains(char) then node.children(char)
          else return false
        }
        .terminator

    def startsWith(prefix: String): Boolean =
      prefix.toCharArray
        .foldLeft(root) { case (node, char) =>
          if node.children.contains(char) then node.children(char)
          else return false
        }
      true

    final case class Node(children: mutable.Map[Char, Node], var terminator: Boolean = false)
