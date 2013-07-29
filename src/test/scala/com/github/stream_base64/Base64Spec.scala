/*
 * Copyright 2013 Eric Olander
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.github.stream_base64

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers


class Base64StreamSpec extends FlatSpec with ShouldMatchers {

  "Base64Stream" should "correctly encode" in {
    val result = Base64Stream.encode("testing".getBytes.toStream)
    result.foreach(print(_))
    println()
  }

  "Base64Stream" should "not stack overflow" in {
    val big = new StringBuilder
    for (i <- 1 to 10000) {
      big ++= i.toString
    }
    val result = Base64Stream.encode(big.toString().getBytes.toStream)
    result.foreach(print(_))
    println()
  }

  "Base64Stream" should "correctly encode and decode to the same value" in {
    Base64Stream.decode(Base64Stream.encode("testing".getBytes.toStream)).toList should equal("testing".getBytes.toList)
  }

  "Base64Stream" should "correctly encode and decode to the same big value" in {
    val big = new StringBuilder
    for (i <- 1 to 50000) {
      big ++= i.toString
    }
    Base64Stream.decode(Base64Stream.encode(big.toString().getBytes.toStream)).toList should equal(big.toString().getBytes.toList)
  }

  "Base64Stream" should "correctly handle empty streams" in {
    Base64Stream.encode(Stream.empty[Byte]) should equal(Stream.empty[Char])
    Base64Stream.decode(Stream.empty[Char]) should equal(Stream.empty[Byte])
  }

  "Base64Stream" should "correctly handle small streams" in {
    Base64Stream.decode(Base64Stream.encode("".getBytes.toStream)).toList should equal("".getBytes.toList)
    Base64Stream.decode(Base64Stream.encode("a".getBytes.toStream)).toList should equal("a".getBytes.toList)
    Base64Stream.decode(Base64Stream.encode("ab".getBytes.toStream)).toList should equal("ab".getBytes.toList)
    Base64Stream.decode(Base64Stream.encode("abc".getBytes.toStream)).toList should equal("abc".getBytes.toList)
    Base64Stream.decode(Base64Stream.encode("abcd".getBytes.toStream)).toList should equal("abcd".getBytes.toList)
  }

}
