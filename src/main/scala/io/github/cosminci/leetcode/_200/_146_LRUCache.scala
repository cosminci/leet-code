package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _146_LRUCache:
  def main(args: Array[String]): Unit =
    val cache = new LRUCache(2)
    cache.put(2, 1)
    cache.put(1, 1)
    cache.put(2, 3)
    cache.put(4, 1)
    println(cache.get(1))
    println(cache.get(2))

  class LRUCache(_capacity: Int):
    class ListNode(val key: Int, val value: Int, var next: ListNode = null, var prev: ListNode = null)
    val kvStore = mutable.Map.empty[Int, ListNode]

    var (headDummy, lastDummy) = (new ListNode(-1, -1), new ListNode(-1, -1))
    headDummy.next = lastDummy
    lastDummy.prev = headDummy

    def get(key: Int): Int =
      kvStore.get(key) match
        case None => -1
        case Some(node) =>
          remove(node)
          add(node)
          node.value

    def put(key: Int, value: Int) =
      val node = new ListNode(key, value)
      kvStore.get(key) match
        case Some(prevNode) =>
          remove(prevNode)
          add(node)
          kvStore.update(key, node)
        case None =>
          if kvStore.size == _capacity then
            val lru = headDummy.next
            kvStore.remove(lru.key)
            remove(lru)
          add(node)
          kvStore.update(key, node)

    def add(node: ListNode): Unit =
      val last = lastDummy.prev
      last.next = node
      lastDummy.prev = node
      node.prev = last
      node.next = lastDummy

    def remove(node: ListNode): Unit =
      val (n, p) = (node.next, node.prev)
      p.next = n
      n.prev = p
