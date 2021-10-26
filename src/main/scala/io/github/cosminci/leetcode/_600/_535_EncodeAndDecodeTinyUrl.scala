package io.github.cosminci.leetcode._600

import scala.collection.mutable
import scala.util.Random

object _535_EncodeAndDecodeTinyUrl:
  class Codec:
    private val encodeMap = mutable.Map.empty[String, String]
    private val decodeMap = mutable.Map.empty[String, String]
    private val urlBase   = "http://tinyurl.com/"
    def encode(longURL: String): String =
      if encodeMap.contains(longURL) then return encodeMap(longURL)

      val code = Random.nextString(10)
      encodeMap.update(longURL, code)
      decodeMap.update(code, longURL)

      s"$urlBase$code"

    def decode(shortURL: String): String =
      decodeMap(shortURL.substring(urlBase.length))
