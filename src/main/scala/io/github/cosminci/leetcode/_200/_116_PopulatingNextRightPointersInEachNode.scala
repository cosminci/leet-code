package io.github.cosminci.leetcode._200

object _116_PopulatingNextRightPointersInEachNode:

  def main(args: Array[String]): Unit =
    val root = new Node(1)
    root.left = new Node(2)
    root.right = new Node(3)
    connect(root)

  def connect(root: Node): Node =
    @annotation.tailrec
    def dfs(levelNodes: Seq[Node]): Unit =
      if levelNodes.nonEmpty then
        (0 until levelNodes.length - 1).foreach(i => levelNodes(i).next = levelNodes(i + 1))
        dfs(levelNodes.flatMap(n => Seq(n.left, n.right).flatMap(Option.apply)))

    Option(root).foreach(r => dfs(Seq(r)))
    root

  class Node(var _value: Int):
    var value: Int  = _value
    var left: Node  = Option.empty.orNull
    var right: Node = Option.empty.orNull
    var next: Node  = Option.empty.orNull
