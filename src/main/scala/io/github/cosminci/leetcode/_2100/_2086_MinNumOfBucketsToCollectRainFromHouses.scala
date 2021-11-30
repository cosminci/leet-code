package io.github.cosminci.leetcode._2100

object _2086_MinNumOfBucketsToCollectRainFromHouses {
  def main(args: Array[String]): Unit = {
    println(minimumBuckets(".HH.H.H.H.."))
  }

  def minimumBuckets(street: String): Int =
    if (street == "H" || street.startsWith("HH") || street.endsWith("HH") || street.contains("HHH")) -1
    else street.replace("H.H", "  ").length - street.replace("H", "").length

}
