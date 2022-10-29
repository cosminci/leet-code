package com.leetcode.cosminci._700

object _676_ImplementMagicDictionary {
  def main(args: Array[String]): Unit = {
    val dict = new MagicDictionary
    dict.buildDict(Array("hello", "hallo", "leetcode"))
    println(dict.search("hello"))
    println(dict.search("hhllo"))
    println(dict.search("hell"))
    println(dict.search("leetcoded"))
  }

  class MagicDictionary {
    private var dictionary: Map[String, Seq[String]] = null

    def buildDict(dictionary: Array[String]) =
      this.dictionary = dictionary.foldLeft(Map.empty[String, Seq[String]]) {
        case (dict, word) =>
          word.indices.foldLeft(dict) {
            case (dict, i) =>
              dict.updatedWith(word.updated(i, '*')) {
                case None => Some(Seq(word))
                case Some(roots) => Some(roots :+ word)
              }
          }
      }

    def search(searchWord: String): Boolean =
      searchWord.indices.exists { i =>
        dictionary.get(searchWord.updated(i, '*')).exists { roots =>
          roots.exists(_ != searchWord)
        }
      }
  }
}
