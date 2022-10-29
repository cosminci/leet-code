package com.leetcode.cosminci._700

import scala.collection.mutable

object _677_MapSumPairs:

  class MapSum():
    class Node(val children: mutable.Map[Char, Node], var value: Int = 0)
    private val root = new Node(children = mutable.Map.empty)

    def insert(key: String, `val`: Int) =
      key.toCharArray
        .foldLeft(root) { (prevNode, char) =>
          prevNode.children.get(char) match
            case None =>
              val newNode = new Node(mutable.Map.empty)
              prevNode.children.update(char, newNode)
              newNode
            case Some(nextNode) =>
              nextNode
        }
        .value = `val`

    def sum(prefix: String): Int =
      def sum(node: Node): Int = node.value + node.children.values.map(sum).sum
      val suffixRoot = prefix.toCharArray.foldLeft(root) { (prevNode, char) =>
        prevNode.children.get(char) match
          case None           => return 0
          case Some(nextNode) => nextNode
      }
      sum(suffixRoot)
