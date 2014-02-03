/* ***** BEGIN LICENSE BLOCK *****
 *
 * Copyright (c) 2001-2014, Kevin Sven Berg. All rights reserved.
 *
 * This package is part of the Bitzguild Collection Distribution
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the <organization> nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ***** END LICENSE BLOCK ***** */

package bitzguild.scollection

import scala.collection.IndexedSeq
import scala.collection.mutable.ArrayBuffer


trait SeriesRing[A] extends BaseSeries[A] {
  def apply(i: Int) = data((cursor + i) % data.size)
}

 /**
  * Abstract Ring Buffer companion reference, shared by inner classes.
  * The reference object provides a lightweight wrapper around a given
  * ring buffer, but with an independent cursor.
  *
  * @param indata any IndexedSeq
  * @param idx Long cursor
  * @tparam A type
  */
abstract class BaseRingBufferRef[A](indata: IndexedSeq[A], protected var idx: Int = 0) 
						extends collection.immutable.IndexedSeq[A] 
						with SeriesRing[A] {

  protected lazy val cmax =(Int.MaxValue / indata.size) * indata.size - indata.size
  protected def data = indata
  def length = data.length
  def cursor = idx
  def cursorMax = cmax
}



