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

import scala.collection.immutable.Stream.Empty

object Base64Stream {

  val indexTable = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toArray
  val reverseIndex = indexTable.zipWithIndex.toMap

  val mask1 = 63 << 18
  val mask2 = 63 << 12
  val mask3 = 63 << 6
  val mask4 = 63

  val revMask1 = 255 << 16
  val revMask2 = 255 << 8
  val revMask3 = 255

  val EQ = '='

  def encode(bytes: Stream[Byte]): Stream[Char] = {
    def lookupBase64Chars(i: Int, n: Int) = {
      val a = indexTable((i & mask1) >> 18)
      val b = indexTable((i & mask2) >> 12)
      if (n == 1) (a, b, EQ, EQ)
      else {
        val c = indexTable((i & mask3) >> 6)
        if (n == 2) (a, b, c, EQ)
        else {
          val d = indexTable(i & mask4)
          (a, b, c, d)
        }
      }
    }
    if (bytes.isEmpty) Stream.empty[Char]
    else {
      val (i, n) = bytes match {
        case a #:: Empty => (a << 16, 1)
        case a #:: b #:: Empty => (((a << 8) + b) << 8, 2)
        case a #:: b #:: c #:: _ => ((((a << 8) + b) << 8) + c, 3)
      }
      val (a, b, c, d) = lookupBase64Chars(i, n)
      a #:: b #:: c #:: d #:: encode(bytes.drop(n))
    }
  }

  def decode(bytes: Stream[Char]): Stream[Byte] = {
    def checkForEqSign(c: Char): Int = if (c != EQ) reverseIndex(c) else 0

    if (bytes.isEmpty) Stream.empty[Byte]
    else {
      // pull 4 characters from the stream
      val a #:: b #:: c #:: d #:: _ = bytes
      val n = if (c == EQ) 1 else if (d == EQ) 2 else 3
      val i = (((((reverseIndex(a) << 6) + reverseIndex(b)) << 6) + checkForEqSign(c)) << 6) + checkForEqSign(d)
      val x = ((i & revMask1) >> 16).toByte
      val y = ((i & revMask2) >> 8).toByte
      val z = (i & revMask3).toByte
      n match {
        case 1 => x #:: Stream.empty[Byte]
        case 2 => x #:: y #:: Stream.empty[Byte]
        case _ => x #:: y #:: z #:: decode(bytes.drop(4))
      }
    }
  }
}
