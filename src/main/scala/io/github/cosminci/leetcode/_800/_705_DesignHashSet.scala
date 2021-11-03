package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _705_DesignHashSet:
  def main(args: Array[String]): Unit =
    val myHashSet = new MyHashSet
    myHashSet.add(1)
    myHashSet.add(2)
    println(myHashSet.contains(1))
    println(myHashSet.contains(3))
    myHashSet.add(2)
    println(myHashSet.contains(2))
    myHashSet.remove(2)
    println(myHashSet.contains(2))

  class MyHashSet():
    private val buckets        = Array.fill(1000)(mutable.ListBuffer.empty[Int])
    def hash(key: Int) = ((key * 1031237) & 1 << 20) % 5

    def add(key: Int) =
      val bucket = hash(key) % buckets.length
      if !buckets(bucket).contains(key) then buckets(bucket).addOne(key)

    def remove(key: Int) =
      val bucket = hash(key) % buckets.length
      val idx    = buckets(bucket).indexOf(key)
      if idx != -1 then buckets(bucket).remove(idx)

    def contains(key: Int): Boolean =
      val bucket = hash(key) % buckets.length
      buckets(bucket).indexOf(key) != -1
