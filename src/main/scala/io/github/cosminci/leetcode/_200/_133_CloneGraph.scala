package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _133_CloneGraph:

  private val clones = mutable.Map.empty[Node, Node]

  def cloneGraph(graph: Node): Node =
    if graph == null then return null
    if clones.contains(graph) then return clones(graph)

    val clone = new Node(graph.value)
    clones.update(graph, clone)
    clone.neighbors = graph.neighbors.map(cloneGraph)
    clones(graph)

  class Node(var _value: Int):
    var value: Int            = _value
    var neighbors: List[Node] = List()
